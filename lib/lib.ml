(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
*)

open Prelude

module Configuration = Configuration
module ErrorHandling = ErrorHandling

module Error = struct
  type t =
    [ `DummyError ] (* For testing *)
end

(* library code for attachment converter goes here *)
module type CONVERT =
sig
  type filepath
  type parsetree
  type htransform = string -> string
  type btransform = string -> string
  val parse : string -> parsetree
  val amap : htransform -> btransform -> parsetree -> parsetree
  val acopy : bool -> (Error.t -> unit) -> Configuration.Formats.t -> parsetree -> (parsetree, Configuration.Formats.error) result
  val to_string : parsetree -> string
  val convert : filepath -> (string -> string)
  val acopy_email : string -> (string -> string) -> string
end

module Conversion_ocamlnet (* : CONVERT *) = struct

  type htransform = string -> string
  type btransform = string -> string
  type filepath =  string
  type parsetree = Netmime.complex_mime_message

  (** parse email string into a parse tree *)
  let parse s =
    let input = new Netchannels.input_string s in
    let ch = new Netstream.input_stream input in
    let f ch =
      Netmime_channels.read_mime_message
        ~multipart_style:`Deep
        ch
    in
    Netchannels.with_in_obj_channel ch f
  (* see
     http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and
     the Netstream input bits, but it's worth keeping an eye on. *)

  (** parse the header field parameters into an association list *)
  let field_params_alist header fieldname =
    let fields =
      String.cuts ~sep:";" (header#field fieldname)
    in
    match tail fields with
    (* Case 1: no params, alist is just field name/value *)
    | Some [] -> [fieldname, (header#field fieldname)]
    (* Case 2: split on "=" then zip together as alist*)
    | Some params ->
       let kvpairs = map (String.cut ~sep:"=") params in
       let process = function
         | key, Some v -> key, v
         | key, None -> key, ""
       in
      map process kvpairs
    | None -> assert false

  (** to_string for Netmime headers *)
  let header_to_string h =
    let buf = Stdlib.Buffer.create 1024 in
    let channel_writer ch =
      Netmime_string.write_header ch (h#fields)
    in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      channel_writer ;
    Stdlib.Buffer.contents buf

  (** from_string for Netmime headers *)
  let header_from_string s =
    let channel_reader ch =
      let stream = new Netstream.input_stream ch in
      Netmime_string.read_header
        ?downcase:(Some false)
        ?unfold:None
        stream
    in
    new Netmime.basic_mime_header
      (Netchannels.with_in_obj_channel
         (new Netchannels.input_string s)
         channel_reader)

let convert script str = let args = split script in
  Unix.Proc.rw args str

let transform hd bd (trans_entry : Configuration.Formats.transform_data) =
  let open Netmime in 
    if trans_entry.variety = NoChange then (hd,`Body bd)
    else 
      let data = bd # value in
      let conv_data = convert trans_entry.shell_command data in
      if trans_entry.variety = DataOnly then
        (hd, `Body (memory_mime_body (conv_data)))
      else 
        let conv_hd = basic_mime_header ["content-type", trans_entry.target_type] in
        (conv_hd, `Body (memory_mime_body (conv_data)))

  (* Notes: Content-Disposition headers provide information about how
     to present a message or a body part. When a body part is to be
     treated as an attached file, the Content-Disposition header will
     include a file name parameter. *)

  (** apply an input function f to every attachment in an email
      parsetree; note that this is not a real functorial map, which
      means we will probably be renaming it in the future *)
  let rec amap f g tree =
    match tree with
    | header, `Body b ->
      let h = header_to_string header in
      if f h = h
      (* only invoke g (the converting function) if f converts the header *)
      then tree
      else header_from_string (f h) ,
            `Body (b#set_value @@ g b#value ; b)
    | header, `Parts p_lst ->
      header, `Parts (List.map (amap f g) p_lst)

(*?(f = Printf.printf)*)
let acopy ?(no_op = true) f dict tree =
  let ( let* ) = Result.(>>=) in
  let err_handler e = if no_op then Error e else (f e; Ok []) in
  match tree with
    | _, `Body _ -> Ok tree 
    | header, `Parts p_lst ->
      let rec copy_or_skip hd lst =
        match lst with
          | (bhd, `Body b) :: rs ->
            Result.on_error
            (let* src = Result.trapc (`DummyError) id (bhd # field "content-type") in
            let* trans_lst = Result.of_option (`DummyError) (Configuration.Formats.Dict.find_opt src dict) in
            let* next_lst = copy_or_skip hd rs in
            let conv_lst = (List.map (transform bhd b) trans_lst) @ [(bhd, `Body b)] in
            Ok (conv_lst @ next_lst)) err_handler
          | (phd, `Parts p) :: rs -> 
            Result.on_error
            (let* conv_lst = copy_or_skip phd p in
            let* next_lst = copy_or_skip hd rs in
            Ok ([(phd, `Parts conv_lst)] @ next_lst)) err_handler
          | _ -> Ok []
      in let* cmp_lst = copy_or_skip header p_lst in
      Ok (header, `Parts cmp_lst)

  (** serialize a parsetree into a string *)
  let to_string tree =
    let (header, _) = tree in
    (* defaulting to a megabyte seems like a nice round number *)
    let n =
      Exn.default
        (1024*1024)
        Netmime_header.get_content_length header
    in
    let buf = Stdlib.Buffer.create n in
    let channel_writer ch =
      Netmime_channels.write_mime_message
        ?crlf:(Some false)
        ch
        tree
    in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      channel_writer ;
    Stdlib.Buffer.contents buf

  (** predicate for email parts that are attachments *)
  let is_attachment tree =
    let header, _ = tree in
    let s = try header # field "content-disposition"
      with Not_found -> ""
    in Strings.prefix "attachment" (String.lowercase_ascii s)

  (** updates the MIME type in a header string *)
  let update_mimetype oldtype newtype hstr =
    let open String in
    let hdr = header_from_string hstr in
    let s = try hdr # field "content-type"
            with Not_found -> ""
    in
    if lowercase_ascii s = lowercase_ascii oldtype
    then (hdr # update_field "content-type" newtype;
          header_to_string hdr)
    else hstr

  let equals_sign star = if star
                         then "*="
                         else "="

  (** updates the name in just the 'filename=' part of the string *)
  let update_filename_string ?(ext="") ?(star=false) str =
    let timestamp () =
      Unix.time ()
      |> string_of_float
      |> fun x -> String.(sub x 0 (length x - 1))
    in
    let new_name ?(star=false) header_key prefix ext =
      String.concat "" [ header_key ;
                         equals_sign star ;
                         prefix ;
                         "_CONVERTED_" ;
                         timestamp () ;
                         ext ; ]
    in 
    match String.split ~sep:"=" str with
    | [ header_key; filename ] ->
       let prefix, e =
         let open Filename in
         remove_extension filename, extension filename
       in
       let extn =
         if e = ""
         then ext
         else e
       in
       new_name ~star:star header_key prefix extn
    | _ -> str

  (** updates the filename within an entire header string; uses
      OCaml's pure regular expression library ocaml-re *)
  let update_filename ?(ext="") ?(star=false) hstr =
    let open Re in
    let open Option in
    let ( let* ) = (>>=) in
    let rex =
      compile @@ seq [ str "filename" ;
                     Re.str (equals_sign star) ;
                     rep1 notnl ;
                     alt [char ';' ; str "\r\n\r\n" ] ]
    in
    let old_string_opt =
      let* grp = exec_opt rex hstr in
      let* matched = Group.get_opt grp 0 in
      Some matched
    in
    let update_header new_h =
      replace_string ~all:false rex ~by:new_h hstr
    in
    let updated_header =
      old_string_opt
      >>| update_filename_string ~ext:ext
      >>| update_header
    in
    match updated_header with
    | None -> hstr
    | Some h -> h

  (** updates both the filename= and the filename*= filenames in an
      attachment *)
  let update_both_filenames ?(ext="") ?(star=false) =
    update_filename ~ext:ext ~star:star
    << update_filename ~ext:ext ~star:true

  (* TODO: Converts all attachments in an email, used by the
     executable code, definition depends on that of a_copy_email *)
  let full_convert_email _ _ = Error `DummyError
end

module REPLTesting = struct

  (* for reference, MBOX From line:
   * From root@gringotts.lib.uchicago.edu Fri Jan 21 11:48:27 2022 *)
  
  include Conversion_ocamlnet

  (** convenience function for unwrapping a `Parts; for REPL only *)
  let unparts = function
    | `Parts plist -> plist
    | _ -> assert false

  (** convenience function for unwrapping a `Body; for REPL only *)
  let unbody = function
    | `Body b -> b
    | _ -> assert false
    
  (** quick access to the PDF attachment part of our example Christmas
      tree email *)
  let xmas_tree () =
    let _, parts = parse (readfile "2843") in
    match unparts parts with
      _ :: attached :: _ -> attached
    | _ -> assert false

  (** function to change the mime type to PDF *)
  let header_func hstring =
    let hs = String.lowercase_ascii hstring in
    match Strings.(
      (substr "content-disposition: attachment;" hs,
       substr "content-type: application/vnd.openxmlformats-officedocument.wordprocessingml.document" hs
      )
          )
    with
      (Some _, Some _) -> update_mimetype
                            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                            "application/pdf"
                            (update_both_filenames ~ext:".pdf" hstring)
    | _ -> hstring (* noop when not a pdf and attachment *)

  (** constant function that returns an example PDF-A as an output;
      assumes you have a file by that name in your project that is a
      PDF-A *)
  let body_func _ = readfile "xmas-PDFA.pdf"

  (** function from filepath pointing at input email to output email
      as a string *)
  (* let docx_convert_test fname =
    let tree = parse (readfile fname) in
    let converted_tree = acopy header_func body_func tree in
    to_string converted_tree *)

  let upcase_header_and_delete_body fname =
    let f = String.uppercase_ascii in
    let g = fun _ -> "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_upcased_and_deleted") (tree |> (amap f g) |> to_string)

  let omit_gore_y_details fname =
    let f = Fun.const "Content-Type: application/json\r\n\r\n" in
    (* let f = fun _ -> "From: Bill Clinton <president@whitehouse.gov>\r\n\r\n" in *)
    let g = Fun.const "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_contented") (tree |> (amap f g) |> to_string)

  (** takes filepath as input and writes a new file with extra spaces
      in the headers *)
  let extra_spaces_in_header fname =
    let double_space c = if c == ' ' then "  " else String.make 1 c in
    let f s = s |> String.foldr (fun c l -> double_space c :: l) [] |> String.concat "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_extra_spaces_in_header") (tree |> (amap f id) |> to_string)
  (* Not sure if this should be possible, may throw an execption *)

  let acopy_email () = assert false
end



(*
 * Copyright (c) 2021 Matt Teichman
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


(*
 * Copyright (c) 2021 Matt Teichman
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
