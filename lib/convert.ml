open Prelude
open Utils

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
  val amap : ?idem:bool -> Configuration.Formats.t -> parsetree -> parsetree
  val acopy : ?idem:bool -> Configuration.Formats.t -> parsetree -> parsetree
  val acopy_email : ?idem:bool -> Configuration.Formats.t -> string -> (string, error) result
  (* val acopy_mbox : ?idem:bool -> Configuration.Formats.t -> in_channel -> (unit, error) result *)
end

module Conversion_ocamlnet = struct
  module Make (C: ATTACHMENT_CONVERTER) = struct
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

    let renamed_file id new_ext filename =
      let base = Filename.remove_extension filename in
      String.concat ""
        [ base;
          "_CONVERTED";
          id;
          new_ext;
        ]

    let create_meta_header_val
      ~source_type:src
      ~target_type:tar
      ~timestamp:ts
      ~conv_id:cid
      ~hashed:hd : Header.Field.Value.t =
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
      let update_cte = update_value Constants.meta_header_cont_dist in
      let update_fields = let open Header in
        update_or_noop update_ct "Content-Type" >>
        update_or_default update_cd (hf_value "attachment") "Content-Disposition" >>
        update_or_noop update_cte "Content-Transfer-Encoding">>
        add
          (create_meta_header_val
            ~source_type:src
            ~target_type:trans_entry.target_type
            ~timestamp:ts
            ~conv_id:trans_entry.convert_id
            ~hashed:(string_of_int hashed_data))
          Constants.meta_header_name
      in
      let* fields = Header.from_assoc_list (hd # fields) in
      let process =
        update_fields >>
        Header.to_assoc_list >>
        Netmime.basic_mime_header >>
        Result.ok
      in
        process fields

    let hash_attach body = Hashtbl.hash (body # value) (* TODO: replace with better hash function *)

    let transform hd bd src trans_entry =
      let open Netmime in
      let open Configuration.Formats in
      let ( let* ) = Result.(>>=) in
      let data = bd # value in
      let hashed_data = hash_attach bd in
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
            let conv =
              let ( let* ) = Option.(>>=) in
              let* hashed = Header.lookup_param header Constants.meta_header_name "original-file-hash" in
              let* id = Header.lookup_param header Constants.meta_header_name "conversion-id" in
                Some (int_of_string hashed, id)
            in
              Option.to_list conv
        | _, `Parts parts ->
            List.flatmap build parts
      in
        build tree

    let filter_converted hashed converted to_convert =
      let rec process l r =
        match l with
        | [] -> r
        | (h, id) :: hs ->
            if h = hashed then
              process hs (List.filter (fun c -> c.Configuration.Formats.convert_id <> id) r)
            else
              process hs r
      in
        process converted to_convert

    let create_multipart_header () = Netmime.basic_mime_header []

    let body_flatmap (f: parsetree -> parsetree list) tree =
      let list_to_tree (l: parsetree list) =
        if len l = 1 then
          List.hd l
        else
          create_multipart_header (), `Parts l
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

    let is_attachment tree =
      let header, _ = tree in
        match Header.lookup_value header "content-disposition" with
        | None -> false
        | Some cd ->
            let s = String.lowercase_ascii cd in
              s = "attachment" || s = "inline"

    let is_converted tree =
      let header, _ = tree in
        Option.something
          (Header.lookup_value header Constants.meta_header_name)

    let amap_or_copy ?(copy=true) ?(idem=true) dict tree =
      let done_converting = if idem then already_converted tree else [] in
      let converted_attachments tree =
        match tree with
        | header, `Body body ->
            if is_attachment (header, `Body body) && (not idem || not (is_converted (header, `Body body))) then
              match Header.lookup_value header "content-type" with
              | None -> []
              | Some ct ->
                 let hashed = hash_attach body in
                 let conversions = Option.default [] (Configuration.Formats.Dict.find_opt ct dict) in
                 let trans_lst = filter_converted hashed done_converting conversions in
                   Result.reduce (List.map (transform header body ct) trans_lst) @
                   if copy || empty trans_lst then [header, `Body body] else []
            else
              [header, `Body body]
        | _, `Parts _ -> [] (* note: case never reached *)
      in
        body_flatmap converted_attachments tree

    (**applies conversions to the attachment elements of the parsetree, replacing the original
          attachment with the converted versions in the returned parsetree*)
    let amap ?(idem=true) dict (tree : parsetree) =
      amap_or_copy ~idem:idem ~copy:false dict tree

    (**applies conversions to the attachment elements of the parsetree, leaving the original
        attachment with the converted versions in the returned parsetree*)
    let acopy ?(idem=true) dict (tree : parsetree) =
      amap_or_copy ~idem:idem dict tree

    let acopy_email ?(idem=true) config email =
      let ( let* ) = Result.bind in
      let* tree = parse email in
      let converted_tree = acopy ~idem:idem config tree in
        Ok (to_string converted_tree)

  end
end

module Converter = Conversion_ocamlnet.Make (Attach_conv)
module _ : CONVERT = Converter
