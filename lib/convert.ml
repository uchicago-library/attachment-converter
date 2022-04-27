open Prelude

module type CONVERT =
sig
  type error
  type filepath
  type parsetree
  type htransform = string -> string
  type btransform = string -> string
  val parse : string -> parsetree
  val amap : ?f:(error -> unit) -> Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy : ?f:(error -> unit) -> Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val to_string : parsetree -> string
  val convert : filepath -> (string -> string)
  val acopy_email : (Configuration.Formats.t) -> string -> (string, error) result
end

module Conversion_ocamlnet = struct

  module Error = struct
    type fatal_error = [ | `DummyError ]
    type log_error   = [ | `EmailParse of string ]

    type t =
      [ | fatal_error
        | log_error
      ]

    let message err =
      match err with
      | `DummyError -> "Dummy error message"
      | `EmailParse msg -> msg

    let is_fatal err =
      match err with
      | #fatal_error -> true
      | _ -> false
  end

  type htransform = string -> string
  type btransform = string -> string
  type filepath   =  string
  type parsetree  = Netmime.complex_mime_message
  type error      = Error.t

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
 let amap_or_copy f dict tree copy =
    let ( let* ) = Result.(>>=) in
    let err_handler part e =
      if   Error.is_fatal e
      then (f e; Ok [part])
      else Error e
    in
    let rec copy_or_skip hd lst =
      match lst with
      | (bhd, `Body b) :: rs ->
          let try_converted =
            let* src       = Result.trapc
                               (`EmailParse "no content-type in header")
                               id
                               (bhd # field "content-type")                                   in
            let  trans_lst = Option.default [] (Configuration.Formats.Dict.find_opt src dict) in
            let* next_lst  = copy_or_skip hd rs                                               in
            let conv_lst   = if   copy
                             then (List.map (transform bhd b) trans_lst) @ [(bhd, `Body b)]
                             else List.map (transform bhd b) trans_lst                        in
            Ok (conv_lst @ next_lst)
          in
            Result.on_error try_converted (err_handler (bhd, `Body b))
      | (phd, `Parts p) :: rs ->
          let try_converted =
            let* conv_lst = copy_or_skip phd p in
            let* next_lst = copy_or_skip hd rs in
              Ok ([(phd, `Parts conv_lst)] @ next_lst)
          in
            Result.on_error try_converted (err_handler (phd, `Parts p))

      | _ -> Ok []
    in
    match tree with
    | _, `Body _ -> Ok tree
    | header, `Parts p_lst ->
        let* cmp_lst = copy_or_skip header p_lst in
        Ok (header, `Parts cmp_lst)

  (**applies conversions to the attachment elements of the parsetree, replacing the original
        attachment with the converted versions in the returned parsetree*)
  let amap ?(f = fun err -> Printf.printf "%s\n" (Error.message err)) dict (tree : parsetree) =
    amap_or_copy f dict tree false

  (**applies conversions to the attachment elements of the parsetree, leaving the original 
      attachment with the converted versions in the returned parsetree*)
  let acopy ?(f = fun err -> Printf.printf "%s\n" (Error.message err)) dict (tree : parsetree) =
    amap_or_copy f dict tree true

  let acopy_email config email =
    let  ( let* )       = Result.(>>=)      in
    let  tree           = parse email       in
    let* converted_tree = acopy config tree in
    Ok (to_string converted_tree)
end

module _ : CONVERT = Conversion_ocamlnet
