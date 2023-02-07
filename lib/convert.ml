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

let gen_multi_header =
  Result.get_ok (Header.of_assoc_list
    [("Content-Type", "multipart/mixed; boundary=attachmentconvertergeneratedboundary") ;
     (Constants.meta_header_name, "generated multipart")])

module type PARSETREE =
sig
  module Error : ERROR

  type t
  val of_string : string -> (t, Error.t) result
  val to_string : t -> string
  val of_list : t list -> t

  type header
  val header : t -> header
  val make_header : Header.t -> header

  val meta_val : header -> Header.Field.Value.t option
  val content_disposition : header -> Header.Field.Value.t option
  val content_type : header -> Header.Field.Value.t option

  type attachment = header Attachment.t
  val is_attachment : t -> bool
  val to_attachment : t -> attachment option
  val of_attachment : attachment -> t
  val attachments : t -> attachment list
  val replace_attachments : (attachment -> attachment list) -> t -> t

  val to_skeleton : t -> Skeleton.t option
end

module Parsetree_utils (T : PARSETREE) = struct
  let attachment_name att = let open Option in
    T.content_disposition (Attachment.header att) >>=
    Header.Field.Value.lookup_param "filename"

  let is_converted =
    Attachment.header >>
    T.meta_val >>
    Option.something
end

module Mrmime_parsetree = struct
  module Error = struct
    type t = [ `EmailParse ]
    let message _ = "Error parsing email"
  end

  type t = Mrmime.Header.t * string Mrmime.Mail.t option
  type header = Mrmime.Header.t
  type attachment = header Attachment.t

  let of_string =
    Angstrom.parse_string ~consume:All Mrmime.Mail.mail >>
    Result.map (fun (h, b) -> (h, Some b)) >>
    Result.witherr (k `EmailParse)

  let to_string = Serialize.(make >> to_string)

  let header = fst
  let make_header=
    Header.to_string >>
    Angstrom.parse_string ~consume:All Mrmime.Header.Decoder.header >>
    Result.get_ok (* TODO *)

  let of_list (l : t list) =
    match len l with
    | 0 -> Mrmime.Header.empty, None
    | 1 -> List.hd l
    | _ -> make_header gen_multi_header, Some (Multipart l)

  let lookup_unstructured name hd = (* TODO: Make it not case-senitive *)
    let ( let* ) = Option.(>>=) in
    let fname = Mrmime.Field_name.v name in
    let* field = List.head (Mrmime.Header.assoc fname hd) in
      match field with
      | Field (_, Unstructured, data) ->
          ( Mrmime.Unstructured.to_string >>
            Header.Field.Value.of_string >>
            Result.to_option
          ) data
      | _ -> None

  let meta_val = lookup_unstructured Constants.meta_header_name
  let content_disposition = lookup_unstructured "Content-Disposition"

  let content_type hd =
    let open Mrmime.Content_type in
    let ct = Mrmime.Header.content_type hd in
    let ty = Type.to_string (ty ct) in
    let subty = Subtype.to_string (subty ct) in
    let mime_ty = ty ^ "/" ^ subty in
    let params =
      let form (name, value) =
        Header.Field.Value.Parameter.make_
          name
          (match value with `String s -> s | `Token s -> s) (* TODO: Is this really necessary? *)
        in
          List.map form (parameters ct)
    in
    Some (Header.Field.Value.make ~params mime_ty)

  let is_attachment =
    header >>
    content_disposition >>
    Option.map
      ( Header.Field.Value.value >>
        String.lowercase_ascii >>
        (fun x -> x = "attachment" || x = "inline")) >>
    Option.default false

  let to_attachment tree =
    let open Mrmime.Mail in
      if is_attachment tree
      then match tree with
        | header, Some (Leaf data) -> Some (Attachment.make header data)
        | _ -> None
      else None

  let of_attachment att =
    let open Mrmime.Mail in
      Attachment.header att,
      Some (Leaf (Attachment.data att))

  let rec attachments tree =
    let open Mrmime.Mail in
      match tree with
      | header, Some (Leaf data) ->
          Option.to_list
            (to_attachment (header, Some (Leaf data)))
      | _, Some (Multipart parts) -> List.flatmap attachments parts
      | _, Some (Message (h, b)) -> attachments (h, Some b)
      | _ -> []

  let rec replace_attachments f tree =
    let open Mrmime.Mail in
      match tree with
      | _, Some (Leaf _) ->
          (match to_attachment tree with
           | None -> tree
           | Some att -> of_list (List.map of_attachment (f att)))
      | header, Some (Message (h, b)) ->
          let (nh, nb) = replace_attachments f (h, Some b) in
            header, Some (Message (nh, Option.get nb)) (* TODO *)
      | header, Some (Multipart parts) ->
          let g t =
            match to_attachment t with
            | Some att -> List.map of_attachment (f att)
            | None -> [replace_attachments f t]
          in
            header, Some (Multipart (List.flatmap g parts))
      | _ -> tree

  let is_converted =
    Attachment.header >>
    meta_val >>
    Option.something

  let attachment_name att = let open Option in
    content_disposition (Attachment.header att) >>=
    Header.Field.Value.lookup_param "filename"

  let rec to_skeleton tree =
    let open Mrmime.Mail in
    let open Skeleton in
    let (_, opt_t) = tree in
      match opt_t with
      | Some (Leaf _) ->
          if is_attachment tree
          then let att = Option.get (to_attachment tree) in
               Some (Attachment (is_converted att, Option.get (attachment_name att)))
          else Some Body
      | Some (Message (h, b)) -> Some (Message (Option.get (to_skeleton (h, Some b))))
      | Some (Multipart parts) -> Some (Multipart (List.map to_skeleton parts))
      | None -> None
end
module _ : PARSETREE = Mrmime_parsetree

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

  let of_list l =
    match len l with
    | 0 -> Netmime.basic_mime_header [], `Body (Netmime.memory_mime_body "")
    | 1 -> List.hd l
    | _ -> make_header gen_multi_header, `Parts l

  let lookup_value field_name header =
    match header # field field_name with
    | exception Not_found -> None
    | exception e -> raise e
    | hv_str -> Result.to_option (Header.Field.Value.of_string hv_str)

  let meta_val = lookup_value Constants.meta_header_name
  let content_disposition = lookup_value "Content-Disposition"
  let content_type = lookup_value "Content-Type"
  (* TODO: make not case sensitive *)

  let is_attachment =
    header >>
    content_disposition >>
    Option.map
      ( Header.Field.Value.value >>
        String.lowercase_ascii >>
        (fun x -> x = "attachment" || x = "inline")
      ) >>
    Option.default false

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

  let rec attachments tree =
    match tree with
    | header, `Body body ->
        Option.to_list
          (to_attachment (header, `Body body))
    | _, `Parts parts ->
      List.flatmap attachments parts

  let rec replace_attachments f tree =
    match tree with
    | _, `Body _ ->
        (match to_attachment tree with
         | None -> tree
         | Some att -> of_list (List.map of_attachment (f att)))
    | header, `Parts parts ->
        let g t =
          match to_attachment t with
          | Some att -> List.map of_attachment (f att)
          | None -> [replace_attachments f t]
        in
          (header, `Parts (List.flatmap g parts))

  let is_converted =
    Attachment.header >>
    meta_val >>
    Option.something

  let attachment_name att = let open Option in
    content_disposition (Attachment.header att) >>=
    Header.Field.Value.lookup_param "filename"

  let rec to_skeleton tree =
    let open Skeleton in
    let (_, tr) = tree in
    match tr with
    | `Body _ ->
        if is_attachment tree
        then
          let att = Option.get (to_attachment tree) in
          Some (Attachment (is_converted att, Option.get (attachment_name att)))
        else Some Body
    | `Parts parts -> Some (Multipart (List.map to_skeleton parts))
end
module _ : PARSETREE = Ocamlnet_parsetree

module Conversion = struct
  module Make (T : PARSETREE) = struct
    include T

    type _params = {
      filename : string ;
      extension : string ;
      source_type : string ;
      target_type : string ;
      timestamp : string ;
      conversion_id : string ;
      hashed : string ;
      script : string ;
    }

    let create_new_header md =
      let meta_header_val =
        let value = "converted" in
        let params =
          [ "source-type", md.source_type;
            "target-type", md.target_type;
            "time-stamp", md.timestamp;
            "conversion-id", md.conversion_id;
            "original-file-hash", md.hashed;
          ]
        in
          Header.Field.Value.make
            ~params:(map (uncurry Header.Field.Value.Parameter.make) params)
            value
      in
      let cd_header_val =
        let value = "attachment" in
        let params =
          [ "filename", md.filename;
          ]
        in
          Header.Field.Value.make
            ~params:(map (uncurry Header.Field.Value.Parameter.make) params)
           value
      in
         Result.get_ok (Header.of_assoc_list
           ([ "Content-Transfer-Encoding", "base64";
              "Content-Type", md.target_type;
              "Content-Disposition", Header.Field.Value.to_string cd_header_val;
              "X-Attachment-Converter", Header.Field.Value.to_string meta_header_val;
            ]))

    let convert_attachment att md =
      let convert_data str =
        let args = split md.script in
          match Unix.Proc.rw args str with
          | exception (Failure msg) ->
              write stderr ("Conversion Failure: Could not run " ^ md.conversion_id ^" script, produced message \"" ^ msg ^ "\"\n"); str (* TODO: Better logging *)
          | exception e -> raise e
          | converted -> converted
      in
      let ts = timestamp () in
      let md =
        { md with
          timestamp = ts ;
          filename = rename_file ts md.extension md.filename ;
        }
      in
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

    let is_converted =
      Attachment.header >>
      T.meta_val >>
      Option.something

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
      let open Configuration.Formats in
      let done_converting = if idem then already_converted tree else [] in
      let process att =
        if not idem || not (is_converted att)
        then match T.content_type (Attachment.header att) with
        | None -> []
        | Some ct_hv ->
            let ct = Header.Field.Value.value ct_hv in
            let hashed = Hashtbl.hash (Attachment.data att) in
            let conversions = conversions dict ct in
            let trans_lst = filter_converted hashed done_converting conversions in
            let create_params trans_data =
              let open Parsetree_utils(T) in
                { source_type = ct;
                  target_type = trans_data.target_type;
                  conversion_id = trans_data.convert_id;
                  script = trans_data.shell_command;
                  hashed = string_of_int hashed;
                  extension = trans_data.target_ext;
                  filename = Option.default "CONVERTED_ATTACHMENT" (attachment_name att);
                  timestamp = "";
                }
            in
              att :: List.map (convert_attachment att << create_params) trans_lst
        else
          [att]
      in
        T.replace_attachments process tree

    let acopy ?(idem=true) dict tree =
      convert_attachments ~idem:idem dict tree

    let acopy_email ?(idem=true) config email =
      let ( let* ) = Result.(>>=) in
      let () = Progress_bar.Printer.print "Parsing email..." in
      let* tree = Result.witherr (k `EmailParse) (T.of_string email) in
      let () =
        let skel_str = Skeleton.to_string (to_skeleton tree) in
        let msg =
          String.concat ""
          [ "Processing email with structure...\n" ;
            "=================================\n" ;
            skel_str ;
            "\n" ;
            "=================================" ;
          ]
        in
        Progress_bar.Printer.print msg
      in
      let converted_tree = acopy ~idem:idem config tree in
      let () =
        let skel_str = Skeleton.to_string (to_skeleton converted_tree) in
        let msg =
          String.concat ""
            [ "Email now has structure...\n" ;
              "=================================\n" ;
               skel_str ;
              "\n" ;
              "=================================\n" ;
              "Processing complete."
            ]
        in
        Progress_bar.Printer.print msg
      in
      Ok (T.to_string converted_tree)

  end
end

module Ocamlnet_Converter = Conversion.Make (Ocamlnet_parsetree)
module Mrmime_Converter = Conversion.Make (Mrmime_parsetree)
module Converter = Mrmime_Converter
