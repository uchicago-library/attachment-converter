open Prelude
open Utils

module Attachment = struct
  type 'a t = {
    header : 'a;
    data : string
  }

  let make header data = {header; data}
  let header a = a.header
  let data a = a.data
end

module type PARSETREE =
sig
  module Error : ERROR

  type t
  val of_string : string -> (t, Error.t) result
  val to_string : t -> string

  type header
  val header : t -> header
  val make_header : Header.t -> header
  val meta_val : header -> Header.Field.Value.t option
  val content_disposition : header -> Header.Field.Value.t option
  val content_type : header -> Header.Field.Value.t option

  type attachment = header Attachment.t
  val attachments : t -> attachment list
  val to_attachment : t -> attachment option
  val of_attachment : attachment -> t
  val replace_attachments : (attachment -> attachment list) -> t -> t
end

module Parsetree_utils (T : PARSETREE) = struct
  let is_attachment tree =
    let dis_fv = T.content_disposition (T.header tree) in
    let dis = Option.map Header.Field.Value.value dis_fv in
    let dis = Option.map String.lowercase_ascii dis in
    match dis with
    | Some "attachment" -> true
    | Some "inline" -> true
    | _ -> false
end

module Ocamlnet_parsetree = struct
  module Error = struct
    type t = [ `EmailParse ]
    let message _ = "Error parsing email"
  end

  type t = Netmime.complex_mime_message
  type header = Netmime.mime_header
  type attachment = header Attachment.t

  let of_string s =
    let input = new Netchannels.input_string s in
    let ch = new Netstream.input_stream input in
    let f ch =
      Netmime_channels.read_mime_message
        ~multipart_style:`Deep
        ch
    in
    Result.trapc
      `EmailParse (* TODO: Better Error Handling *)
      (Netchannels.with_in_obj_channel ch)
      f

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

  let header = fst
  let make_header =
    Header.to_assoc_list >>
    Netmime.basic_mime_header

  let lookup_value header field_name =
    match header # field field_name with
    | exception Not_found -> None
    | exception e -> raise e
    | hv_str -> Result.to_option (Header.Field.Value.parse hv_str)

  let meta_val hd = lookup_value hd "X-Attachment-Converter" (* TODO: make not case sensitive *)
  let content_disposition hd = lookup_value hd "Content-Disposition"
  let content_type hd = lookup_value hd "Content-Type"

  let is_attachment tree =
    let dis_fv = content_disposition (header tree) in
    let process = Header.Field.Value.value >> String.lowercase_ascii in
    let dis = Option.map process dis_fv in
    match dis with
    | Some "attachment" -> true
    | Some "inline" -> true
    | _ -> false

  let to_attachment tree =
    if is_attachment tree then
      (match tree with
        | header, `Body body -> Some (Attachment.make header (body # value))
        | _ -> None)
    else
      None

  let of_attachment a =
    ( Attachment.header a,
      `Body (Netmime.memory_mime_body (Attachment.data a))
    )

  let attachments tree =
    let rec build tree =
      match tree with
      | header, `Body data -> Option.to_list (to_attachment (header, `Body data))
      | _, `Parts parts ->
          List.flatmap build parts
    in
      build tree

  let create_multipart_header () = Netmime.basic_mime_header []

  let replace_attachments f tree =
    let list_to_tree l =
      let l = List.map of_attachment l in
      if len l = 1 then
        List.hd l
      else
        create_multipart_header (), `Parts l
    in
    let rec process tree =
      match tree with
      | header, `Body body ->
          (match to_attachment (header, `Body body) with
          | None -> header, `Body body
          | Some at -> list_to_tree (f at))
      | header, `Parts parts ->
          let g t =
            match t with
            | h, `Body b ->
                (match to_attachment (h, `Body b) with
                | None -> [h, `Body b]
                | Some at -> List.map of_attachment (f at))
            | h, `Parts p -> [process (h, `Parts p)]
          in
            (header, `Parts (List.flatmap g parts))
    in
      process tree
end
module _ : PARSETREE = Ocamlnet_parsetree

module Conversion = struct
  module Make (T : PARSETREE) = struct
    (* module Error = struct
      type t = T.Error.t
      let message = T.Error.message
    end *)
    module Error = T.Error

    type _params = {
      filename : string;
      source_type : string;
      target_type : string;
      timestamp : string;
      conversion_id : string;
      hashed : string;
      script : string;
    }

    let create_new_header md =
      let meta_header_val =
        let params =
          [ "source-type", md.source_type;
            "target-type", md.target_type;
            "time-stamp", md.timestamp;
            "conversion-id", md.conversion_id;
            "original-file-hash", md.hashed;
          ]
        in
          Header.Field.Value.hf_value
            ~params:(map (uncurry Header.Field.Value.Parameter.param) params)
            "Converted"
      in
      let cd_header_val =
        let params =
          [ "filename", md.filename;
            "filename*", md.filename;
          ]
        in
          Header.Field.Value.hf_value
            ~params:(map (uncurry Header.Field.Value.Parameter.param) params)
           "Attachment"
      in
         Result.get_ok (Header.from_assoc_list
           ([ "Content-Type", md.target_type;
              "Content-Disposition", Header.Field.Value.to_string cd_header_val;
              "Content-Encoding", "Base64";
              "X-Attachment-Converter", Header.Field.Value.to_string meta_header_val;
           ]))

    let convert_attachment att md =
      let convert_data str =
        let args = split md.script in
          match Unix.Proc.rw args str with
          | exception (Failure msg) ->
              write stderr ("Conversion Failure: " ^ msg); str (* TODO: Better logging *)
          | exception e -> raise e
          | converted -> converted
      in
      let md = { md with timestamp = timestamp ()} in
      let new_header = T.make_header (create_new_header md) in
      let converted_data = convert_data (Attachment.data att) in
        Attachment.make new_header converted_data

    let already_converted tree =
      let attachments = T.attachments tree in
      let process att =
        let ( let* ) = Option.(>>=) in
        let* meta = T.meta_val (T.header (T.of_attachment att)) in
        let* hashed = Header.Field.Value.lookup_param "original-file-hash" meta in
        let* id = Header.Field.Value.lookup_param "conversion-id" meta in
          Some (int_of_string hashed, id)
      in
        Option.reduce (List.map process attachments)

    let is_converted att =
      let header = T.header (T.of_attachment att) in
        Option.something (T.meta_val header)

    let filter_converted hashed converted to_convert =
      let open Configuration.Formats in
      let rec process l r =
        match l with
        | [] -> r
        | (h, id) :: hs ->
            if h = hashed
            then process hs (List.filter (fun c -> c.convert_id <> id) r)
            else process hs r
      in
        process converted to_convert

    let convert_attachments ?(idem=true) dict tree =
      let done_converting = if idem then already_converted tree else [] in
      let process att =
        if not idem || not (is_converted att)
        then match T.content_type (Attachment.header att) with
        | None -> []
        | Some ct_hv ->
            let ct = Header.Field.Value.value ct_hv in
            let hashed = Hashtbl.hash (Attachment.data att) in
            let conversions =
              Option.default []
                (Configuration.Formats.Dict.find_opt ct dict)
            in
            let trans_lst = filter_converted hashed done_converting conversions in
            let create_params trans_data =
              let open Configuration.Formats in
              { source_type = ct;
                target_type = trans_data.target_type;
                conversion_id = trans_data.convert_id;
                script = trans_data.shell_command;
                hashed = string_of_int hashed;
                timestamp = "";
                filename = "";
              }
            in
              att :: List.map (create_params >> convert_attachment att) trans_lst
        else
          [att]
      in
        T.replace_attachments process tree

    let acopy ?(idem=true) dict tree =
      convert_attachments ~idem:idem dict tree

    let acopy_email ?(idem=true) config email =
      let ( let* ) = Result.(>>=) in
      let* tree = Result.witherr (k `EmailParse) (T.of_string email) in
      let converted_tree = acopy ~idem:idem config tree in
        Ok (T.to_string converted_tree)

    let acopy_mbox ?(idem=true) config in_chan =
      let converter (fromline, em) =
        match acopy_email ~idem:idem config em with
        | Ok converted -> fromline ^ "\n" ^ converted
        | Error _ -> write stderr "Conversion failure\n"; fromline ^ "\n" ^ em (* TODO: better logging *)
      in
        Ok (Mbox.convert_mbox in_chan converter)
  end
end

module Ocamlnet_converter = Conversion.Make (Ocamlnet_parsetree)
module Converter = Ocamlnet_converter
