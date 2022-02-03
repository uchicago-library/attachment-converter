(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
*)


open Prelude

module type CONVERT =
sig
  type filepath
  type parsetree
  type htransform = string -> string
  type btransform = string -> string
  val parse : string -> parsetree
  val amap : htransform -> btransform -> parsetree -> parsetree
  val acopy : htransform -> btransform -> parsetree -> parsetree
  val to_string : parsetree -> string
  val convert : filepath list -> (string -> string)
  val acopy_email : string -> (string -> string) -> string
end

module Conversion_ocamlnet = struct
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
    (* case 2: split on "=" then zip together as alist*)
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
        ?unfold:(Some false)
        stream
    in
    new Netmime.basic_mime_header
      (Netchannels.with_in_obj_channel
         (new Netchannels.input_string s)
         channel_reader)

  (* Notes: Content-Disposition headers provide information about how to present
     a message or a body part. When a body part is to be treated as an attached
     file, the Content-Disposition header will include a file name parameter. *)

  (** apply an input function f to every attachment in an email parsetree; note
     that this is not a real functorial map, which means we will probably be
     renaming it in the future *)
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

  (** apply an input function f to every attachment in an email parsetree and
     put the result next to the original *)
  let rec acopy f g tree =
    match tree with
    (* leave input email unchanged if it isn't multipart *)
    | _, `Body _ -> tree 
    | header, `Parts p_lst ->
       let copy_or_skip part =
         match part with
         | header, `Body b ->
            let hstring = header_to_string header in
            (* do nothing to the body if the header transform leaves the header
               alone *)
            if f hstring = hstring
            then [ part ]
            else [ header_from_string (f hstring),
                   `Body (b#set_value (g b#value); b); part]
         | _ -> [ acopy f g part ]
       in header, `Parts List.(concat_map copy_or_skip p_lst)

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

  (** putting this off till issues 7 and 9 *)
  let convert () = assert false

  (** predicate for email parts that are attachments *)
  let is_attachment tree =
    let header, _ = tree in
    let s = try header # field "content-disposition"
      with Not_found -> ""
    in Strings.prefix "attachment" (String.lowercase_ascii s)

  (** updates the MIME type in a header string *)
  let update_mimetype newtype hstr =
    let open String in
    let hdr = header_from_string hstr in
    let s = try hdr # field "Content-Type" with Not_found -> "" in
    if Stdlib.String.(trim s = trim newtype)
    then hstr
    else begin
        hdr#update_field "Content-Type" newtype ;
        header_to_string hdr
      end

  (** updates the filename in a header string *)
  let update_filename ?(ext="") hstr =
    let open String in
    let open Filename in
    let timestamp () =
      Unix.time ()
      |> string_of_float
      |> fun x -> sub x 0 (length x - 1)
    in
    match substr "filename=" hstr with
    | Some lo ->
       let hi = lo + try Option.get (substr "\r\n" hstr#.(lo,0)) with
                       Invalid_argument _ -> assert false (* should never happen if libpst does its job *)
       in
       let old_name =    (* offsets are for escaped quotation marks and/or a ';',
                            which is only required if more params follow the filename param.*)
         if mem ';' hstr#.(lo, hi)
         then hstr#.(lo + 10, hi - 2)
         else hstr#.(lo + 10, hi - 1)
       in
       let new_name =
         String.join ~sep:"" [
             (remove_extension old_name) ;
             ".CONVERTED." ;
             timestamp () ;
             "." ;
             (extension old_name) ;
             ext ;
           ]
       in replace old_name new_name hstr
    | _ -> assert false


  (* TESTS *)

  (* TODOs
   *
   * - [x] have pdf_convert_test update the Content-Type header
   * - [~] put the line breaks back in the semicolon-separated header field
   *   parameters (i.e. filename=etc) in the output
   * - [x] see if the result will open in Apple Mail
   *
   * to make the output into an MBOX, put this at the top:
   *
From root@gringotts.lib.uchicago.edu Fri Jan 21 11:48:27 2022
   *)

  let docx_convert_test fname =
    let header_func hstring =
      let hs = String.lowercase_ascii hstring in
      match Strings.(
          (substr "content-disposition: attachment;" hs,
           substr "content-type: application/vnd.openxmlformats-officedocument.wordprocessingml.document" hs)
        )
      with
        (Some _, Some _) -> update_mimetype
                              (* "application/vnd.openxmlformats-officedocument.wordprocessingml.document" (\* note: should we make this optional? how much could we infer from config etc *\) *)
                              "application/pdf"
                              (update_filename ~ext:".pdf" hstring)
      | _ -> hstring (* noop when not a pdf and attachment *)
    in
    let body_func bstr =
      let open Prelude.Unix.Proc in
      let tmpname = fname ^ "_extracted_tmp.docx" in
      (writefile ~fn:(tmpname) bstr; read ["pandoc"; "--from=docx"; "--to=pdf"; "--pdf-engine=xelatex"; tmpname])
    in
    let tree = parse (readfile fname) in
    let converted_tree = amap header_func body_func tree
    in writefile ~fn:(fname ^ "_docxmas_saved") (to_string converted_tree)


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

  let acopy_email () = assert false

end

module REPLTesting = struct
  include Conversion_ocamlnet
        
  let unparts = function
    | `Parts plist -> plist
    | _ -> assert false
         
  let unbody = function
    | `Body b -> b
    | _ -> assert false
         
  let xmas_tree () =
    let _, parts = parse (readfile "../2843") in
    match unparts parts with
      _ :: attached :: _ -> attached
    | _ -> assert false
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
