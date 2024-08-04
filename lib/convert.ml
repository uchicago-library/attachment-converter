open Prelude
open Utils
module Trace = Error.T

module Attachment = struct
  type 'a t = { header : 'a; data : string }

  let make header data = { header; data }
  let header a = a.header
  let data a = a.data
end

let gen_multi_header =
  let open Header in
  of_list
    [ Field.make "Content-Type"
        (Field.Value.make "multipart/mixed"
           ~params:
             [ Field.Value.Parameter.make "boundary"
                 "attachmentconvertergeneratedboundary"
             ] );
      Field.make Constants.meta_header_name
        (Field.Value.make "generated multipart")
    ]

module type LINE_FEED = sig
  type t = Dos | Unix

  val figure_out_line_ending : string -> t
  val remove_crs : string -> string
  val add_crs : string -> string
end

module Line_feed : LINE_FEED = struct
  type t = Dos | Unix

  let figure_out_line_ending email_string =
    let open Prelude.String in
    let not_cr c = not (contains "\r\n" c) in
    match dropwhile not_cr email_string with
    | "" -> Unix
    | nonempty -> begin
      match nonempty.[0] with
      | '\r' -> Dos
      | _ -> Unix
    end

  let remove_crs str =
    let b = Buffer.create 0 in
    let mk_new_string () =
      let each_char c =
        match c with
        | '\r' -> ()
        | _ -> Buffer.add_char b c
      in
      String.iter each_char str
    in
    mk_new_string () ;
    Buffer.contents b

  let add_crs str =
    let b = Buffer.create 0 in
    let mk_new_string () =
      let each_char c =
        match c with
        | '\n' ->
          Buffer.add_char b '\r' ;
          Buffer.add_char b '\n'
        | _ -> Buffer.add_char b c
      in
      String.iter each_char str
    in
    mk_new_string () ;
    Buffer.contents b
end

module type PARSETREE = sig
  type t

  val of_string_line_feed :
    string -> (t * Line_feed.t, Error.t) result

  val to_string_line_feed :
    ?line_feed:Line_feed.t -> t -> string

  val of_list : t list -> t

  type header

  val header : t -> header
  val make_header : Header.t -> header
  val meta_val : header -> Header.Field.Value.t option

  val content_disposition :
    header -> Header.Field.Value.t option

  val content_type : header -> Header.Field.Value.t option

  type attachment = header Attachment.t

  val is_attachment : t -> bool
  val to_attachment : t -> attachment option
  val of_attachment : attachment -> t
  val attachments : t -> attachment list

  val replace_attachments :
    (attachment -> attachment list) -> t -> t

  val to_skeleton : t -> Skeleton.t
end

module Parsetree_utils (T : PARSETREE) = struct
  let attachment_name att =
    let open Option in
    T.content_disposition (Attachment.header att)
    >>= Header.Field.Value.lookup_param "filename"

  let is_converted =
    Attachment.header >> T.meta_val >> Option.something

  let mime_type_opt =
    let open Option in
    Attachment.header
    >> T.content_type
    >=> ( Header.Field.Value.value
        >> Mime_type.of_string
        >> Result.to_option )
end

module Mrmime_parsetree = struct
  module E = Parsetree_error

  exception HeaderRepresentationError

  type t = Mrmime.Header.t * string Mrmime.Mail.t
  type header = Mrmime.Header.t
  type attachment = header Attachment.t

  let of_string =
    Angstrom.parse_string ~consume:All
      (Mrmime.Mail.mail None)
    >> flip Result.on_error
         (k (Trace.throw E.Smart.parse_err))

  let of_string_line_feed email_str =
    let ( let* ) = Result.( >>= ) in
    let lf_type =
      Line_feed.figure_out_line_ending email_str
    in
    let processed =
      match lf_type with
      | Unix -> Line_feed.add_crs email_str
      | Dos -> email_str
    in
    let* parsed = of_string processed in
    Ok (parsed, lf_type)

  let to_string = Serialize.(make >> to_string)

  let to_string_line_feed ?(line_feed = Line_feed.Unix) tree
      =
    let preliminary_output = to_string tree in
    match line_feed with
    | Unix -> Line_feed.remove_crs preliminary_output
    | Dos -> preliminary_output

  let header = fst

  let make_header h =
    let decoder = Mrmime.Header.Decoder.header None in
    let of_string =
      Angstrom.parse_string ~consume:All decoder
    in
    match of_string (Header.to_string h) with
    | Ok h -> h
    | Error _ -> raise HeaderRepresentationError

  let header_only h = (h, Mrmime.Mail.Leaf "")
  let with_some_body (a, b) = (a, Some b)

  let of_list (l : t list) =
    match len l with
    | 0 -> header_only Mrmime.Header.empty
    | 1 -> List.hd l
    | _ ->
      ( make_header gen_multi_header,
        Multipart (List.map with_some_body l) )

  let lookup_unstructured name hd =
    (* TODO: Make it not case-senitive *)
    let ( let* ) = Option.( >>= ) in
    let fname = Mrmime.Field_name.v name in
    let* field = List.head (Mrmime.Header.assoc fname hd) in
    match field with
    | Field (_, Unstructured, data) ->
      ( Prettym.to_string ~margin:Constants.max_line_length
          Mrmime.Unstructured.Encoder.unstructured
      >> Header.Field.Value.of_string
      >> fun x -> Some x )
        data
    | _ -> None

  let meta_val =
    lookup_unstructured Constants.meta_header_name

  let content_disposition =
    lookup_unstructured "Content-Disposition"

  let content_type hd =
    let open Mrmime.Content_type in
    let ct = Mrmime.Header.content_type hd in
    let ty = Type.to_string (ty ct) in
    let subty = Subtype.to_string (subty ct) in
    let mime_ty = ty ^ "/" ^ subty in
    let params =
      let form (name, value) =
        Header.Field.Value.Parameter.make name
          ( match value with
          | `String s -> s
          | `Token s -> s )
        (* TODO: Is this really necessary? *)
      in
      List.map form (parameters ct)
    in
    Some (Header.Field.Value.make ~params mime_ty)

  let is_attachment =
    header
    >> content_disposition
    >> Option.map
         ( Header.Field.Value.value
         >> String.lowercase_ascii
         >> fun x -> x = "attachment" || x = "inline" )
    >> Option.default false

  let to_attachment tree =
    let open Mrmime.Mail in
    if is_attachment tree
    then
      match tree with
      | header, Leaf data ->
        Some (Attachment.make header data)
      | _ -> None
    else None

  let of_attachment att =
    let open Mrmime.Mail in
    (Attachment.header att, Leaf (Attachment.data att))

  let rec attachments tree =
    let open Mrmime.Mail in
    match tree with
    | header, Leaf data ->
      Option.to_list (to_attachment (header, Leaf data))
    | _, Multipart parts ->
      let with_data ats (h, d) =
        match d with
        | None -> ats
        | Some d -> (h, d) :: ats
      in
      List.flatmap attachments
        (List.foldl with_data [] parts)
    | _, Message (h, b) -> attachments (h, b)

  let rec replace_attachments f tree =
    let open Mrmime.Mail in
    match tree with
    | _, Leaf _ ->
      Option.either
        (f >> List.map of_attachment >> of_list)
        tree
        (to_attachment tree)
    | header, Message (h, b) ->
      let nh, nb = replace_attachments f (h, b) in
      (header, Message (nh, nb))
    | header, Multipart parts ->
      let walk_replace (h, b) =
        match b with
        | None -> [ (h, None) ]
        | Some b ->
          Option.either
            (f >> List.map (of_attachment >> with_some_body))
            [ with_some_body (replace_attachments f (h, b))
            ]
            (to_attachment (h, b))
      in
      (header, Multipart (List.flatmap walk_replace parts))

  let is_converted =
    Attachment.header >> meta_val >> Option.something

  let attachment_name att =
    let open Option in
    content_disposition (Attachment.header att)
    >>= Header.Field.Value.lookup_param "filename"

  let rec to_skeleton tree =
    let open Mrmime.Mail in
    let open Skeleton in
    let _, t = tree in
    match t with
    | Leaf _ -> (
      match to_attachment tree with
      | None -> Body
      | Some att ->
        Attachment
          ( is_converted att,
            Option.default "NONAME" (attachment_name att) )
      )
    | Message (h, b) -> Message (to_skeleton (h, b))
    | Multipart parts ->
      let walk (h, b) =
        match b with
        | None -> None
        | Some b -> Some (to_skeleton (h, b))
      in
      Multipart (List.map walk parts)
end

module _ : PARSETREE = Mrmime_parsetree

module Ocamlnet_parsetree = struct
  module E = Parsetree_error

  type t = Netmime.complex_mime_message
  type header = Netmime.mime_header
  type attachment = header Attachment.t

  let of_string s =
    let input = new Netchannels.input_string s in
    let ch = new Netstream.input_stream input in
    let f ch =
      Netmime_channels.read_mime_message
        ~multipart_style:`Deep ch
    in
    Result.trapc
      (Trace.new_list E.Smart.parse_err)
      (* TODO: Better Error Handling *)
      (Netchannels.with_in_obj_channel ch)
      f

  let of_string_line_feed email_str =
    let ( let* ) = Result.( >>= ) in
    let lf_type =
      Line_feed.figure_out_line_ending email_str
    in
    let* processed = of_string email_str in
    Ok (processed, lf_type)

  let to_string_line_feed ?(line_feed = Line_feed.Unix) tree
      =
    let header, _ = tree in
    (* defaulting to a megabyte seems like a nice round
       number *)
    let n =
      Exn.default (1024 * 1024)
        Netmime_header.get_content_length header
    in
    let buf = Stdlib.Buffer.create n in
    let crlf =
      match line_feed with
      | Dos -> Some true
      | Unix -> Some false
    in
    let channel_writer ch =
      Netmime_channels.write_mime_message ?crlf ch tree
    in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      channel_writer ;
    Stdlib.Buffer.contents buf

  let header = fst

  let make_header =
    Header.to_assoc_list >> Netmime.basic_mime_header

  let of_list l =
    match len l with
    | 0 ->
      ( Netmime.basic_mime_header [],
        `Body (Netmime.memory_mime_body "") )
    | 1 -> List.hd l
    | _ -> (make_header gen_multi_header, `Parts l)

  let lookup_value field_name header =
    match header#field field_name with
    | exception Not_found -> None
    | exception e -> raise e
    | hv_str -> Some (Header.Field.Value.of_string hv_str)

  let meta_val = lookup_value Constants.meta_header_name

  let content_disposition =
    lookup_value "Content-Disposition"

  let content_type = lookup_value "Content-Type"
  (* TODO: make not case sensitive *)

  let is_attachment =
    header
    >> content_disposition
    >> Option.map
         ( Header.Field.Value.value
         >> String.lowercase_ascii
         >> fun x -> x = "attachment" || x = "inline" )
    >> Option.default false

  let to_attachment tree =
    if is_attachment tree
    then
      match tree with
      | header, `Body body ->
        Some (Attachment.make header body#value)
      | _ -> None
    else None

  let of_attachment a =
    ( Attachment.header a,
      `Body (Netmime.memory_mime_body (Attachment.data a))
    )

  let rec attachments tree =
    match tree with
    | header, `Body body ->
      Option.to_list (to_attachment (header, `Body body))
    | _, `Parts parts -> List.flatmap attachments parts

  let rec replace_attachments f tree =
    match tree with
    | _, `Body _ -> (
      match to_attachment tree with
      | None -> tree
      | Some att -> of_list (List.map of_attachment (f att))
      )
    | header, `Parts parts ->
      let g t =
        match to_attachment t with
        | Some att -> List.map of_attachment (f att)
        | None -> [ replace_attachments f t ]
      in
      (header, `Parts (List.flatmap g parts))

  let is_converted =
    Attachment.header >> meta_val >> Option.something

  let attachment_name att =
    let open Option in
    content_disposition (Attachment.header att)
    >>= Header.Field.Value.lookup_param "filename"

  let rec to_skeleton tree =
    let open Skeleton in
    let _, tr = tree in
    match tr with
    | `Body _ -> (
      match to_attachment tree with
      | None -> Body
      | Some att ->
        Attachment
          ( is_converted att,
            Option.default "NONAME" (attachment_name att) )
      )
    | `Parts parts ->
      Multipart
        (List.map (Option.some << to_skeleton) parts)
end

module _ : PARSETREE = Ocamlnet_parsetree

module Conversion = struct
  module Make (T : PARSETREE) = struct
    include T

    type _params =
      { filename : string;
        extension : string;
        source_type : string;
        target_type : string;
        timestamp : string;
        conversion_id : string;
        hashed : string;
        script : string
      }

    let create_new_header md =
      let open Header in
      let meta_header_val =
        let value = "converted" in
        let params =
          [ ("source-type", md.source_type);
            ("target-type", md.target_type);
            ("time-stamp", md.timestamp);
            ("conversion-id", md.conversion_id);
            ("original-file-hash", md.hashed)
          ]
        in
        Field.Value.make
          ~params:
            (map
               (uncurry Field.Value.Parameter.make)
               params )
          value
      in
      let cd_header_val =
        let value = "attachment" in
        let params = [ ("filename", quoted md.filename) ] in
        Field.Value.make
          ~params:
            (map
               (uncurry Field.Value.Parameter.make)
               params )
          value
      in
      of_list
        [ Field.make "Content-Transfer-Encoding"
            (Field.Value.make "base64");
          Field.make "Content-Type"
            (Field.Value.make md.target_type);
          Field.make "Content-Disposition" cd_header_val;
          Field.make "X-Attachment-Converter"
            meta_header_val
        ]

    let convert_attachment att pbar md =
      let convert_data str =
        let args = split md.script in
        match Unix.Proc.rw args str with
        | exception Failure msg ->
          write stderr
            ( "Conversion Failure: Could not run "
            ^ md.conversion_id
            ^ " script, produced message \"" ^ msg ^ "\"\n"
            ) ;
          None (* TODO: Better logging *)
        | exception e -> raise e
        | converted -> Some converted
      in
      let ts = timestamp () in
      let md =
        { md with
          timestamp = ts;
          filename =
            rename_file ts md.extension md.filename pbar
        }
      in
      let new_header =
        T.make_header (create_new_header md)
      in
      let ( let* ) = Option.( >>= ) in
      let* converted_data =
        convert_data (Attachment.data att)
      in
      Some (Attachment.make new_header converted_data)

    let hashed_data = Attachment.data >> Hashtbl.hash

    let already_converted tree =
      let attachments = T.attachments tree in
      let process att =
        let ( let* ) = Option.( >>= ) in
        let* meta = T.meta_val (Attachment.header att) in
        let* hashed =
          Header.Field.Value.lookup_param
            "original-file-hash" meta
        in
        let* id =
          Header.Field.Value.lookup_param "conversion-id"
            meta
        in
        Some (int_of_string hashed, id)
      in
      Option.reduce (List.map process attachments)

    let filter_converted already_converted conversions att =
      let open Configuration.TransformData in
      let convs_of_att =
        filter
          (fst >> ( = ) (hashed_data att))
          already_converted
      in
      let pred c =
        let rec go l =
          match l with
          | [] -> true
          | (_, conv_id) :: hs ->
            convert_id c <> conv_id && go hs
        in
        go convs_of_att
      in
      filter pred conversions

    let to_convert ?(idem = true) config tree att =
      let open Configuration.Formats in
      let open Parsetree_utils (T) in
      match mime_type_opt att with
      | None -> []
      | Some mty ->
        let conversions = conversions config mty in
        if idem
        then conversions
        else
          filter_converted
            (already_converted tree)
            conversions att

    let convert_attachments ?(idem = true) dict tree pbar =
      let open Configuration in
      let open Parsetree_utils (T) in
      let process att =
        if (not idem) || not (is_converted att)
        then
          match mime_type_opt att with
          | None -> []
          | Some mty ->
            let conversions =
              Formats.conversions dict mty
            in
            let trans_lst =
              if idem
              then conversions
              else
                filter_converted
                  (already_converted tree)
                  conversions att
            in
            let create_params trans_data =
              { source_type = Mime_type.to_string mty;
                target_type =
                  Mime_type.to_string
                    (TransformData.target_type trans_data);
                conversion_id =
                  TransformData.convert_id trans_data;
                script =
                  TransformData.shell_command trans_data;
                hashed = string_of_int (hashed_data att);
                extension =
                  TransformData.target_ext trans_data;
                filename =
                  Option.default "CONVERTED_ATTACHMENT"
                    (attachment_name att);
                timestamp = ""
              }
            in
            att
            :: ( List.map
                   ( convert_attachment att pbar
                   << create_params )
                   trans_lst
               |> Option.reduce )
        else [ att ]
      in
      T.replace_attachments process tree

    let acopy ?(idem = true) dict tree pbar =
      convert_attachments ~idem dict tree pbar

    let attachments_to_convert ?(idem = true) config tree =
      let open Parsetree_utils (T) in
      let match_convs att =
        let dname = "UNNAMED_ATTACHMENT" in
        let add_name c =
          (Option.default dname (attachment_name att), c)
        in
        map add_name (to_convert ~idem config tree att)
      in
      concat_map match_convs (attachments tree)

    let display_conv (name, conv) =
      let open Configuration.TransformData in
      String.concat ""
        [ name;
          " to ";
          Mime_type.to_string (target_type conv);
          " (";
          convert_id conv;
          ")"
        ]

    let acopy_email ?(idem = true) config email pbar =
      let ( let* ) = Result.( >>= ) in
      let () =
        Progress_bar.Printer.print "Parsing email..." pbar
      in
      let* tree, line_feed =
        Trace.with_error Parsetree_error.Smart.parse_err
          (T.of_string_line_feed email)
      in
      let convs =
        attachments_to_convert ~idem config tree
      in
      if List.empty convs
      then
        let () =
          Progress_bar.Printer.print
            "Nothing to convert...\nProcessing complete."
            pbar
        in
        Ok email
      else
        let () =
          let skel_str =
            Skeleton.to_string (to_skeleton tree)
          in
          let msg =
            String.concat ""
              [ "Processing email with structure...\n";
                "=================================\n";
                skel_str;
                "\n";
                "================================="
              ]
          in
          Progress_bar.Printer.print msg pbar
        in
        let () =
          let lines =
            [ "Conversions to perform...\n";
              "=================================\n";
              String.join ~sep:"\n" (map display_conv convs);
              "\n";
              "================================="
            ]
          in
          let msg = String.concat "" lines in
          Progress_bar.Printer.print msg pbar
        in
        let converted_tree = acopy ~idem config tree pbar in
        let () =
          let skel_str =
            Skeleton.to_string (to_skeleton converted_tree)
          in
          let msg =
            String.concat ""
              [ "Email now has structure...\n";
                "=================================\n";
                skel_str;
                "\n";
                "=================================\n";
                "Processing complete."
              ]
          in
          Progress_bar.Printer.print msg pbar
        in
        Ok (T.to_string_line_feed ~line_feed converted_tree)
  end
end

module type CONVERTER = sig
  include PARSETREE

  val acopy_email :
    ?idem:bool ->
    Configuration.Formats.t ->
    string ->
    out_channel ->
    (string, Error.t) result
end

module Ocamlnet_Converter =
  Conversion.Make (Ocamlnet_parsetree)

module Mrmime_Converter = Conversion.Make (Mrmime_parsetree)
module Converter = Ocamlnet_Converter
