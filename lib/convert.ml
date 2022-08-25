open Prelude

module type ATTACHMENT_CONVERTER =
sig
  val convert: string -> (string -> string)
end

module Attach_conv: ATTACHMENT_CONVERTER = struct
  let convert script str =
    let args = split script in
      match Unix.Proc.rw args str with
      | exception (Failure msg) ->
          write stderr ("Conversion Failure: " ^ msg); str (* TODO: Better logging *)
      | exception e -> raise e
      | converted -> converted
end

module type CONVERT =
sig
  type error
  type parsetree
  val parse : string -> (parsetree, error) result
  val to_string : parsetree -> string
  val amap : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy_email : Configuration.Formats.t -> string -> (string, error) result
  val acopy_mbox : Configuration.Formats.t -> in_channel -> (unit, error) result
end

module Conversion_ocamlnet_F (C: ATTACHMENT_CONVERTER) = struct

  module Error = struct
    type t = [
      | `EmailParse (* TODO: More data *)
      | `MissingContentType
      | Header.Field.Value.Error.t
    ]

    let message err =
      match err with
      | `EmailParse -> "Error parsing the given email"
      | `MissingContentType -> "Content type missing"
      | #Header.Field.Value.Error.t as e ->
          Header.Field.Value.Error.message e
  end

  type parsetree  = Netmime.complex_mime_message
  type error      = Error.t

  (** parse email string into a parse tree *)
  let parse s =
    let input = new Netchannels.input_string s in
    let ch = new Netstream.input_stream input  in
    let f ch =
      Netmime_channels.read_mime_message
        ~multipart_style:`Deep
        ch                                     in
    (* TODO: more fine-grained error handling *)
    Result.trapc
      `EmailParse
      (Netchannels.with_in_obj_channel ch)
      f

  (* see
     http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and
     the Netstream input bits, but it's worth keeping an eye on. *)

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

  let timestamp () =
    Unix.time ()
      |> string_of_float
      |> fun x -> String.(sub x 0 (length x - 1))

  (** predicate for email parts that are attachments *)
  let is_attachment tree =
    let ( let* ) = Result.(>>=) in
    let header, _ = tree in
      match header # field "content-disposition" with
      | exception Not_found -> Ok false
      | exception e -> raise e (* TODO: Better logging and error handling *)
      | hd ->
          let* hv = Header.Field.Value.parse hd in
          let s = String.lowercase_ascii (Header.Field.Value.value hv) in
            Ok (s = "attachment" || s = "inline")

  let renamed_file id new_ext filename =
    let base = Filename.remove_extension filename in
    String.concat ""
      [ base;
        "_CONVERTED";
        id;
        new_ext;
      ]

  let meta_header_name = "X-Attachment-Converter" (* TODO: MOVE SOMEWHERE ELSE, CODE SMELL *)

  let create_meta_header_val src tar ts cid hd : Header.Field.Value.t =
    let params =
      [ "source-type", src;
        "target-type", tar;
        "time-stamp", ts;
        "conversion-id", cid;
        "original-file-hash", hd;
      ]
    in
      Header.Field.Value.hf_value
        ~params:(map (uncurry Header.Field.Value.Parameter.param) params)
        "converted"

  let updated_header hd src trans_entry hashed_data =
    let open Header.Field.Value in
    let open Configuration.Formats in
    let ( let* ) = Result.(>>=) in
    let ts = timestamp () in
    let rename = renamed_file ts trans_entry.target_ext in
    let update_ct =
      update_value trans_entry.target_type >>
      map_val "name" rename
    in
    let update_cd =
      update_value "attachment" >>
      map_val "filename" rename >>
      map_val "filename*" rename
    in
    let update_cte = update_value "base64" in
    let update_fields = let open Header in
      update_or_noop update_ct "Content-Type" >>
      update_or_default update_cd (hf_value "attachment") "Content-Disposition" >>
      update_or_noop update_cte "Content-Transfer-Encoding">>
      add
        (create_meta_header_val
          src
          trans_entry.target_type
          ts
          trans_entry.convert_id
          (string_of_int hashed_data))
        meta_header_name
    in
    let* fields = Header.from_assoc_list (hd # fields) in
    let process =
      update_fields >>
      Header.to_assoc_list >>
      Netmime.basic_mime_header >>
      Result.ok
    in
      process fields
(*
  let updated_header hd src trans_entry hashed_data =
    let open Configuration.Formats in
    let open Header.Field.Value in
    let ( let* ) = Result.(>>=) in
    let ts = timestamp () in
    let new_ext = trans_entry.target_ext in
    let* new_ct =
      let process =
        update_value trans_entry.target_type >>
        map_val "name" (renamed_file ts new_ext) >>
        to_string
      in
      let* ct_hv = Header.Field.Value.parse (hd # field "content-type") in
        Ok (process ct_hv)
    in
    let* new_dis =
      let process =
        update_value "attachment" >>
        map_val "filename" (renamed_file ts new_ext) >>
        map_val "filename*" (renamed_file ts new_ext) >>
        to_string
      in
      match hd # field "content-disposition" with
      | exception Not_found -> Ok "attachment"
      | exception e -> raise e (* TODO: better logging *)
      | dis ->
          match Header.Field.Value.parse dis with
          | Ok dis_hv -> Ok (process dis_hv)
          | Error _ -> Ok dis
    in
    let meta_header_hv =
      Header.Field.Value.to_string
        (create_meta_header_val
          src
          trans_entry.target_type
          ts
          trans_entry.convert_id
          (string_of_int hashed_data))
     in
     let fields =
      Assoc.(
        replace ("Content-Type", new_ct) >>
        replace ("Content-Transfer-Encoding", "base64") >>
        replace ("Content-Disposition", new_dis) >>
        add meta_header_name meta_header_hv)
          (hd # fields)
    in
      Ok (Netmime.basic_mime_header fields)
*)

  let transform hd bd src trans_entry =
    let open Netmime in
    let open Configuration.Formats in
    let ( let* ) = Result.(>>=) in
    let data = bd # value in
    let hashed_data = Hashtbl.hash data in
    let* conv_hd = updated_header hd src trans_entry hashed_data in
    let conv_data = C.convert trans_entry.shell_command data in
      Ok (match trans_entry.variety with
        | NoChange -> hd, `Body bd
        | DataOnly -> hd, `Body (memory_mime_body conv_data)
        | DataAndHeader -> conv_hd, `Body (memory_mime_body conv_data))

    (* Notes: Content-Disposition headers provide information about how
        to present a message or a body part. When a body part is to be
        treated as an attached file, the Content-Disposition header will
        include a file name parameter. *)

  let conversion_id header =
    match header # field meta_header_name with
    | exception Not_found -> None
    | exception e -> raise e
    | mh ->
        let hv = Header.Field.Value.unsafe_parse mh in
          Header.Field.Value.lookup_param "conversion-id" hv

  let content_type header =
    let ( let* ) = Result.bind in
      match header # field "content-type" with
      | exception Not_found -> Error `MissingContentType
      | exception e -> raise e (* TODO: Better logging and error handling *)
      | ct ->
          let* hv = Header.Field.Value.parse ct in
          Ok (Header.Field.Value.value hv)

  let already_converted tree =
    let ( let* ) = Result.bind in
    let rec build lst tree =
      match tree with
      | header, `Body body ->
          (match conversion_id header with
          | Some id -> Ok ((Hashtbl.hash body, id) :: lst)
          | None -> Ok lst)
      | _, `Parts parts ->
          let f curr next =
            let* l = curr in
            let* r = build lst next in
              Ok (l @ r)
          in
            List.foldl f (Ok []) parts
    in
      build [] tree

(*
  let amap_or_copy dict tree copy =
    let ( let* ) = Result.bind in
    let rec process tree =
      match tree with
      | header, `Body body ->
          let* check = is_attachment header in
          if check then
            let* source_type = content_type header in
            let trans_lst = Configuration.Formats.transformations dict source_type in
            let converted_attachments =
              Ok (Result.reduce (List.map (transform header body source_type) trans_lst)) in
              (
          else
            [(body, `Body body)]
      | header, `Parts parts ->
          (* map stuff over *)
*)

  let amap_or_copy dict tree copy =
    let ( let* ) = Result.bind in
    let rec copy_or_skip lst =
      match lst with
      | (bhd, `Body b) :: rs ->
          let* check = is_attachment (bhd, `Body b) in
          let* converted =
            if   check
            then match bhd # field "content-type" with
                 | exception Not_found -> Ok [(bhd, `Body b)]
                 | exception e -> raise e (* TODO: Better logging and error handling *)
                 | ct ->
                     let* hv = Header.Field.Value.parse ct in
                     let src = Header.Field.Value.value hv in
                     let trans_lst = Option.default []
                                       (Configuration.Formats.Dict.find_opt src dict)
                     in
                       Ok ((Result.reduce (List.map (transform bhd b src) trans_lst)) @
                         if copy || empty trans_lst then [(bhd, `Body b)] else []) (* TODO: better logging *)
            else Ok [(bhd, `Body b)]
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
    let ( let* ) = Result.bind in
    let* tree = parse email in
    let* converted_tree = acopy config tree in
      Ok (to_string converted_tree)

  let acopy_mbox config in_chan =
    let converter (fromline, em) =
      match acopy_email config em with
      | Ok converted -> fromline ^ "\n" ^ converted
      | Error _ -> write stderr "Conversion failure\n"; fromline ^ "\n" ^ em (* TODO: better logging *)
    in
      Ok (Mbox.convert_mbox in_chan converter)
end

module Conversion_ocamlnet = Conversion_ocamlnet_F (Attach_conv)
module _ : CONVERT = Conversion_ocamlnet
