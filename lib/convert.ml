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
  val amap : Configuration.Formats.t -> parsetree -> parsetree
  val acopy : Configuration.Formats.t -> parsetree -> parsetree
  val acopy_email : Configuration.Formats.t -> string -> (string, error) result
  val acopy_mbox : Configuration.Formats.t -> in_channel -> (unit, error) result
end

module Conversion_ocamlnet_F (C: ATTACHMENT_CONVERTER) = struct

  module Error = struct
    type t = [
      | `EmailParse (* TODO: More data *)
      | Header.Field.Value.Error.t
    ]

    let message err =
      match err with
      | `EmailParse -> "Error parsing the given email"
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

  let is_attach tree =
    let header, _ = tree in
      match Header.lookup_value header "content-disposition" with
      | None -> false
      | Some cd ->
          let s = String.lowercase_ascii cd in
            s = "attachment" || s = "inline"

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

  let update_header hd src trans_entry hashed_data =
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

  let transform hd bd src trans_entry =
    let open Netmime in
    let open Configuration.Formats in
    let ( let* ) = Result.(>>=) in
    let data = bd # value in
    let hashed_data = Hashtbl.hash data in
    let* conv_hd = update_header hd src trans_entry hashed_data in
    let conv_data = C.convert trans_entry.shell_command data in
      Ok (match trans_entry.variety with
        | NoChange -> hd, `Body bd
        | DataOnly -> hd, `Body (memory_mime_body conv_data)
        | DataAndHeader -> conv_hd, `Body (memory_mime_body conv_data))

    (* Notes: Content-Disposition headers provide information about how
        to present a message or a body part. When a body part is to be
        treated as an attached file, the Content-Disposition header will
        include a file name parameter. *)

  let already_converted tree =
    let rec build tree =
      match tree with
      | header, `Body _ ->
          let p =
            let ( let* ) = Option.(>>=) in
            let* hashed = Header.lookup_param header meta_header_name "original-file-hash" in
            let* id = Header.lookup_param header meta_header_name "conversion-id" in
              Some (int_of_string hashed, id)
          in
            Option.to_list p
      | _, `Parts parts ->
          List.flatmap build parts
    in
      build tree

  let is_converted tree =
    let header, _ = tree in
      Option.something
        (Header.lookup_value header meta_header_name)

  let filter_converted hashed converted to_convert =
    let rec process l r =
      match l with
      | [] -> r
      | (h, id) :: hs ->
          if h = hashed then
            let _ = write stderr "got here" in
            process hs (List.filter (fun c -> c.Configuration.Formats.convert_id <> id) r)
          else
            process hs r
    in
      process converted to_convert

  let body_flatmap (f: parsetree -> parsetree list) tree =
    let create_multipart_header () = Netmime.basic_mime_header [] (* TODO *) in
    let list_to_tree (l: parsetree list) =
      Option.default
        (create_multipart_header (), `Parts l)
        (List.head l)
    in
    let rec process tree =
      match tree with
      | header, `Body body -> list_to_tree (f (header, `Body body))
      | header, `Parts parts ->
          let g t =
            match t with
            | h, `Body b -> f (h, `Body b)
            | h, `Parts p -> [process (h, `Parts p)]
          in
            (header, `Parts (List.flatmap g parts))
    in
      process tree

  let amap_or_copy dict ?(copy=true) ?(idem=true) tree =
    let done_converting = if idem then already_converted tree else [] in
    let converted_attachments tree =
      match tree with
      | header, `Body body ->
          if is_attach (header, `Body body) && (not idem || not (is_converted (header, `Body body))) then
            match Header.lookup_value header "content-type" with
            | None -> []
            | Some ct ->
               let hashed = Hashtbl.hash (body # value) in (* TODO: pass this to transform *)
               let conversions = Option.default [] (Configuration.Formats.Dict.find_opt ct dict) in
               let trans_lst = filter_converted hashed done_converting conversions in
                 Result.reduce (List.map (transform header body ct) trans_lst) @
                 if copy || empty trans_lst then [header, `Body body] else []
          else
            [header, `Body body]
      | _, `Parts _ -> [] (* case never reached *)
    in
      body_flatmap converted_attachments tree

(*    let ( let* ) = Result.bind in
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
*)

  (**applies conversions to the attachment elements of the parsetree, replacing the original
        attachment with the converted versions in the returned parsetree*)
  let amap dict (tree : parsetree) =
    amap_or_copy dict ~copy:false tree

  (**applies conversions to the attachment elements of the parsetree, leaving the original
      attachment with the converted versions in the returned parsetree*)
  let acopy dict (tree : parsetree) =
    amap_or_copy dict tree

  let acopy_email config email =
    let ( let* ) = Result.bind in
    let* tree = parse email in
    let converted_tree = acopy config tree in
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
