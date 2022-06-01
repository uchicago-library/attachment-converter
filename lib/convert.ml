open Prelude

module type CONVERT =
sig
  type error
  type parsetree
  type htransform = string -> string
  type btransform = string -> string
  val parse : string -> (parsetree, error) result
  val amap : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val to_string : parsetree -> string
  val convert : string -> (string -> string)
  val acopy_email : (Configuration.Formats.t) -> string -> (string, error) result
end

module Conversion_ocamlnet = struct

  module Error = struct
    type t = [
      | `EmailParse (* TODO: More data *)
    ]

    let message err =
      match err with
      | `EmailParse -> "Error parsing the given email"
  end

  type htransform = string -> string
  type btransform = string -> string
  type parsetree  = Netmime.complex_mime_message
  type error      = Error.t

  (** parse email string into a parse tree *)
  let parse s =
    let input = new Netchannels.input_string s in
    let ch = new Netstream.input_stream input in
    try
      let f ch =
        Netmime_channels.read_mime_message
          ~multipart_style:`Deep
          ch
      in
      Ok (Netchannels.with_in_obj_channel ch f)
    with _ ->
      Error `EmailParse (* TODO: more fine-grained error handling *)

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

  let convert script str =
    try
      let args = split script in
      Unix.Proc.rw args str
    with (Failure msg) ->
      write stderr ("Conversion Failure: " ^ msg); str (* TODO: Better logging *)

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

  let timestamp () =
    Unix.time ()
      |> string_of_float
      |> fun x -> String.(sub x 0 (length x - 1))

  let new_filename ?(tstamped=true) ?(star=false) header_key prefix ext =
    let newname = String.concat "" [ prefix                                      ;
                                     "_CONVERTED"                                ;
                                     if tstamped then "_" ^ timestamp () else "" ;
                                     ext                                         ;
                                   ]
    in
    if   star
    then header_key ^ "*="  ^ newname
    else header_key ^ "=\"" ^ newname ^ "\""

  (** updates the name in just the 'filename=' part of the string *)
  let update_filename_string ?(tstamped=true) ?(star=false) ext str =
    match String.split ~sep:(equals_sign star) str with
    | [ header_key; filename ] ->
       let name   = if star then filename else String.trim "\"" filename in
       let prefix = Filename.remove_extension name                       in
       new_filename ~tstamped:tstamped ~star:star header_key prefix ext
    | _ -> str

  (** updates the filename within an entire header string; uses
      OCaml's pure regular expression library ocaml-re *)
  let update_filename ?(ext="") ?(tstamped=false) ?(star=false) hstr =
    let open Re in
    let open Option in
    let ( let* ) = (>>=) in
    let pattern =
      if   star
      then [ str "filename" ;
             Re.str "*=" ;
             rep1 (diff any (set "\n;")) ; ]
      else [ str "filename" ;
             Re.str "=" ;
             char '"' ;
             rep1 (diff any (set "\n\"")) ;
             char '"' ; ]
    in
    let rex = compile @@ seq pattern in
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
      >>| update_filename_string ~tstamped:tstamped ~star:star ext
      >>| update_header
    in
    match updated_header with
    | None -> hstr
    | Some h -> h

  (** updates both the filename= and the filename*= filenames in an
      attachment *)
  let update_both_filenames ?(ext="") ?(tstamped=true) =
    update_filename ~ext:ext ~tstamped:tstamped ~star:false
    << update_filename ~ext:ext ~tstamped:tstamped ~star:true

  let transform hd bd trans_entry =
    let open Netmime                                       in
    let open Configuration.Formats                         in
    let data      = bd # value                             in
    let conv_data = convert trans_entry.shell_command data in
    let conv_hd   =
      let ct     = ("Content-Type", trans_entry.target_type) in
      let cte    = ("Content-Transfer-Encoding", "base64")   in
      let fields =
        try
          let dis         = hd # field "content-disposition"   in
          let ext         = trans_entry.target_ext             in
          let updated_dis = update_both_filenames ~ext:ext dis in
          [ct; cte; ("Content-Disposition", updated_dis)]
        with Not_found ->
          (* TODO: Better error handling *)
          [ct; cte]                                          in
      basic_mime_header fields                             in
    match trans_entry.variety with
    | NoChange      -> hd,      `Body bd
    | DataOnly      -> hd,      `Body (memory_mime_body conv_data)
    | DataAndHeader -> conv_hd, `Body (memory_mime_body conv_data)

    (* Notes: Content-Disposition headers provide information about how
        to present a message or a body part. When a body part is to be
        treated as an attached file, the Content-Disposition header will
        include a file name parameter. *)

  let amap_or_copy dict tree copy =
    let ( let* ) = Result.bind in
    let rec copy_or_skip lst =
      match lst with
      | (bhd, `Body b) :: rs ->
          let converted = if is_attachment (bhd, `Body b) then
            try
              let src = bhd # field "content-type" in
              let  trans_lst = Option.default []
                                 (Configuration.Formats.Dict.find_opt src dict)
              in
              if   copy || empty trans_lst
              then (List.map (transform bhd b) trans_lst) @ [(bhd, `Body b)]
              else List.map (transform bhd b) trans_lst
            with Not_found ->
              (* TODO: better logging *)
              [(bhd, `Body b)]
          else [(bhd, `Body b)]
          in
          let* next_lst  = copy_or_skip rs in
          Ok (converted @ next_lst)
      | (phd, `Parts p) :: rs ->
          let* conv_lst = copy_or_skip p in
          let* next_lst = copy_or_skip rs in
          Ok ((phd, `Parts conv_lst) :: next_lst)
      | [] -> Ok []
    in
    match tree with
    | _, `Body _ -> Ok tree (* TODO: Handle case with no body *)
    | header, `Parts p_lst ->
        let* cmp_lst = copy_or_skip p_lst in
        Ok (header, `Parts cmp_lst)

  (**applies conversions to the attachment elements of the parsetree, replacing the original
        attachment with the converted versions in the returned parsetree*)
  let amap dict (tree : parsetree) =
    amap_or_copy dict tree false

  (**applies conversions to the attachment elements of the parsetree, leaving the original 
      attachment with the converted versions in the returned parsetree*)
  let acopy dict (tree : parsetree) =
    amap_or_copy dict tree true

  let acopy_email config email =
    let  ( let* )       = Result.bind       in
    let*  tree          = parse email       in
    let* converted_tree = acopy config tree in
    Ok (to_string converted_tree)

  let acopy_mbox config mbox =
    let open Mbox in
    let module T = IteratorFunctions (MBoxIterator (LineIterator)) (StringLog) in
    let converter (_, em) =
      match acopy_email config em with
      | Ok converted -> converted
      | Error _      -> write stderr "Conversion failure"; em (* TODO: better error handling *) in
    Ok (T.convert mbox () converter)
end

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

  let test_acopy () =
    let ( let* ) = Result.(>>=) in
    let tree = tree () in
    let* dict = Result.witherrc (`DummyError) (dict ()) in
    acopy dict tree

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
    let _, parts = Result.get_ok (parse (readfile "2843")) in
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
