open Prelude

module type CONVERT =
sig
  type log
  type error
  type filepath
  type parsetree
  type htransform = string -> string
  type btransform = string -> string
  val parse : string -> parsetree (* TODO: make output a result *)
  val amap : log -> Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy : log -> Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val to_string : parsetree -> string
  val convert : filepath -> (string -> string) (* TODO: make output a result *)
  val acopy_email : log -> (Configuration.Formats.t) -> string -> (string, error) result
end

module Conversion_ocamlnetF (L : Logs.LOG) = struct

  module Error = struct
    type t = [ | `DummyError ] (* TODO: more possible error variants *)

    let message err =
      match err with
      | `DummyError -> "Dummy error message"
  end

  type htransform = string -> string
  type btransform = string -> string
  type filepath   = string
  type parsetree  = Netmime.complex_mime_message
  type error      = Error.t
  type log        = L.t

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

  let transform hd bd trans_entry =
    let open Netmime in
    let open Configuration.Formats in
    let data = bd # value in
    let conv_data = convert trans_entry.shell_command data in
    let conv_hd =
      let hd_str = header_to_string (basic_mime_header ["content-type", trans_entry.target_type]) in
      header_from_string (update_both_filenames ~ext:trans_entry.target_type hd_str)
    in
    match trans_entry.variety with
    | NoChange -> hd,`Body bd
    | DataOnly -> hd, `Body (memory_mime_body (conv_data))
    | DataAndHeader -> conv_hd, `Body (memory_mime_body (conv_data))

    (* Notes: Content-Disposition headers provide information about how
        to present a message or a body part. When a body part is to be
        treated as an attached file, the Content-Disposition header will
        include a file name parameter. *)

  let amap_or_copy (log : L.t) dict tree copy =
    let ( let* ) = Result.bind in
    let rec copy_or_skip hd lst =
      match lst with
      | (bhd, `Body b) :: rs ->
          let converted =
            try
              let src = bhd # field "content-type" in
              let  trans_lst = Option.default []
                                 (Configuration.Formats.Dict.find_opt src dict)
              in
              if   copy || empty trans_lst
              then (List.map (transform bhd b) trans_lst) @ [(bhd, `Body b)]
              else List.map (transform bhd b) trans_lst
            with _ ->
              L.write log "no content-type in header\n";
              [(bhd, `Body b)]
          in
          let* next_lst  = copy_or_skip hd rs in
          Ok (converted @ next_lst)
      | (phd, `Parts p) :: rs ->
          let* conv_lst = copy_or_skip phd p in
          let* next_lst = copy_or_skip hd rs in
          Ok ([(phd, `Parts conv_lst)] @ next_lst)
      | _ -> Ok []
    in
    match tree with
    | _, `Body _ -> Ok tree
    | header, `Parts p_lst ->
        let* cmp_lst = copy_or_skip header p_lst in
        Ok (header, `Parts cmp_lst)

  (**applies conversions to the attachment elements of the parsetree, replacing the original
        attachment with the converted versions in the returned parsetree*)
  let amap log dict (tree : parsetree) =
    amap_or_copy log dict tree false

  (**applies conversions to the attachment elements of the parsetree, leaving the original 
      attachment with the converted versions in the returned parsetree*)
  let acopy log dict (tree : parsetree) =
    amap_or_copy log dict tree true

  let acopy_email log config email =
    let  ( let* )       = Result.bind           in
    let  tree           = parse email           in
    let* converted_tree = acopy log config tree in
    Ok (to_string converted_tree)
end

module Conversion_ocamlnet  = Conversion_ocamlnetF (Logs.OutChannelLog)
module _ : CONVERT = Conversion_ocamlnet

module REPLTesting = struct

  (* for reference, MBOX From line:
   * From root@gringotts.lib.uchicago.edu Fri Jan 21 11:48:27 2022 *)

  include Conversion_ocamlnet

  let unparts_opt = function 
    | Ok (_, `Parts lst) -> lst
    | _ -> assert false

  let get_header = function
    | (hd, _) -> hd

  let parse_file str = 
    Configuration.ParseConfig.parse_config_file str

  let print_error err = Printf.printf "%s\n" (Error.message err)

  let tree () = let pdf_h = Netmime.basic_mime_header ["content-type", "application/pdf"] in
    let pdf_data = Unix.Proc.read ["cat"; "/Users/cormacduhamel/Downloads/Nietzsche.pdf"] in
    (pdf_h, `Parts [(pdf_h, `Body (Netmime.memory_mime_body pdf_data))])

  let err_tree () = let pdf_h = Netmime.basic_mime_header ["content-type", "application/pdf"] in
  let txt_h = Netmime.basic_mime_header ["content-type", "text/plain"] in
  let pdf_data = Unix.Proc.read ["cat"; "/Users/cormacduhamel/Downloads/Nietzsche.pdf"] in
  (pdf_h, `Parts [(pdf_h, `Body (Netmime.memory_mime_body pdf_data)); (txt_h, `Body (Netmime.memory_mime_body "str_data"))])

  let dict () = 
    parse_file "/Users/cormacduhamel/sample_refer.txt"

  let test_acopy log () =
    let ( let* ) = Result.(>>=) in
    let tree = tree () in
    let* dict = Result.witherrc (`DummyError) (dict ()) in
    acopy log dict tree

  (** convenience function for unwrapping a `Parts; for REPL only *)
  let unparts = function
    | `Parts plist -> plist
    | _ -> assert false

  (** convenience function for unwrapping a `Body; for REPL only *)
  let unbody = function
    | `Body b -> b
    | _ -> assert false

  let to_mbox ?(escape=false) ?(eol="\n") =
    let fromline = "From jorge@babel.lib Thu Aug 24 12:00:00 1899" ^ eol in
    if escape then
      let open Strings in
      let escape_froms = replace (eol ^ "From ") (eol ^ ">From ") << replace (eol ^ ">From ") (eol ^ ">>From ") in
      fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) "" << map escape_froms
    else
      fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) ""
         
  let get_body = function
  | (_, bd) -> unbody bd
    
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

  (* let upcase_header_and_delete_body fname =
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
  (* Not sure if this should be possible, may throw an execption *) *)
end
