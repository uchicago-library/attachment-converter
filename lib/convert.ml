open Prelude

module type CONVERT =
sig
  type error
  type parsetree
  val convert : string -> (string -> string)
  val parse : string -> (parsetree, error) result
  val amap : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val acopy : Configuration.Formats.t -> parsetree -> (parsetree, error) result
  val to_string : parsetree -> string
  val acopy_email : Configuration.Formats.t -> string -> (string, error) result
  val acopy_mbox : Configuration.Formats.t -> in_channel -> (unit, error) result
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

  type parsetree  = Netmime.complex_mime_message
  type error      = Error.t

  let convert script str =
    try
      let args = split script in
      Unix.Proc.rw args str
    with (Failure msg) ->
      write stderr ("Conversion Failure: " ^ msg); str (* TODO: Better logging *)

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

  module HeaderValue = struct

    type parameter = {
      attr: string;
      value: string;
      quotes: bool;
    }

    type value = {
      head: string;
      params: parameter list;
    }

    let rec split_on_sep lst sep =
      match lst with
      | [] -> ([], [])
      | x :: xs ->
        if prefix sep lst then
          ([], drop (length sep) lst)
        else
          let (left, right) = split_on_sep xs sep in
            (x :: left, right)

    let opt_split_on_sep lst sep =
      let (left, right) = split_on_sep lst sep in
        match right with
        | [] -> None
        | _  -> Some (left, right)

    let opt_split_on_sep_str str sep =
      let open String in
      match opt_split_on_sep (explode str) (explode sep) with
      | None -> None
      | Some (left, right) -> Some (implode left, implode right)

    let is_quoted str = String.prefix "\"" str && String.suffix "\"" str
    let unquoted str = String.trim "\"" str
    let quoted str = "\"" ^ str ^ "\""

    let parse_eq_sep str =
      let ( let* ) = Option.(>>=) in
      let* (attr, value) = opt_split_on_sep_str str "=" in
        Some (if is_quoted value then
          { attr = attr;
            value = unquoted value;
            quotes = true;
          }
        else
          { attr = attr;
            value = value;
            quotes = false;
          })

    let parse str =
      let open String in
      let vs = split ~sep:";" str in
      let vs = List.map (trim whitespace) vs in (* not sure if trimming is necessary or should be done *)
      let rec red ls =
        match ls with
        | [] -> Some []
        | None :: _ -> None
        | Some p :: ps -> Option.map (fun xs -> p :: xs) (red ps)
      in
        match vs with
        | [] -> None
        | head :: params ->
            match red (List.map parse_eq_sep params) with
            | None -> None
            | Some params ->
                Some {
                  head = head;
                  params = params;
                }

      let unsafe_parse = Option.get << parse

      let to_string { head ; params } =
        let f curr { attr; value ; quotes } =
            curr ^
            ";\n\t" ^
            attr ^ "=" ^
            (if quotes then quoted value else value)
        in
          match params with
          | [] -> head ^ ";"
          | _  -> List.foldl f head params

      let lookup_param attr hv =
        let rec lookup attr ls =
          match ls with
          | [] -> None
          | p :: ps ->
              if p.attr = attr then
                Some p.value
              else
                lookup attr ps
        in
          lookup attr hv.params

      let update_head new_head hv =
        { hv with head = new_head }

      let update_param attr new_val hv =
        let rec update attr new_val ls =
          match ls with
          | [] -> []
          | p :: ps ->
              if p.attr = attr then
                { p with value = new_val } :: ps
              else
                p :: update attr new_val ps
        in
          { hv with params = update attr new_val hv.params }

      let map_param attr f hv =
        match (lookup_param attr hv) with
        | None -> hv
        | Some value -> update_param attr (f value) hv
  end

  let timestamp () =
    Unix.time ()
      |> string_of_float
      |> fun x -> String.(sub x 0 (length x - 1))

  (** predicate for email parts that are attachments *)
  let is_attachment tree =
    let header, _ = tree in
    let s =
      try
        String.lowercase_ascii
          (HeaderValue.unsafe_parse (header # field "content-disposition")).head
      with Not_found -> ""
    in s = "attachment" || s = "inline"

  let renamed_file id new_ext filename =
    let base = Filename.remove_extension filename in
    String.concat ""
      [ base ;
        "_CONVERTED" ;
        id;
        new_ext ;
      ]

  let transform hd bd trans_entry =
    let open Netmime in
    let open Configuration.Formats in
    let open HeaderValue in
    let data = bd # value in
    let conv_data = convert trans_entry.shell_command data in
    let new_ext = trans_entry.target_ext in
    let ts = timestamp () in
    let conv_hd =
      let ct =
        ("Content-Type",
          (unsafe_parse >>
          (update_head trans_entry.target_type) >>
          (map_param "name" (renamed_file ts new_ext)) >>
          to_string) (hd # field "content-type")) in
      let cte = ("Content-Transfer-Encoding", "base64") in
      let fields =
        try
          let updated_dis =
            (unsafe_parse >>
            update_head "attachment" >>
            map_param "filename" (renamed_file ts new_ext) >>
            map_param "filename*" (renamed_file ts new_ext) >>
            to_string) (hd # field "content-disposition") in
          [ct; cte; ("Content-Disposition", updated_dis)] (* TODO: Other header fields? *)
        with Not_found ->
          (* TODO: Better error handling *)
          [ct; cte] in
        basic_mime_header fields in
    match trans_entry.variety with
    | NoChange -> hd, `Body bd
    | DataOnly -> hd, `Body (memory_mime_body conv_data)
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
              let src = (HeaderValue.unsafe_parse (bhd # field "content-type")).head in
              let trans_lst = Option.default []
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

  let acopy_mbox config in_chan =
    let converter (fromline, em) =
      match acopy_email config em with
      | Ok converted -> fromline ^ "\n" ^ converted
      | Error _ -> write stderr "Conversion failure\n"; fromline ^ "\n" ^ em (* TODO: better logging *)
    in
      Ok (Mbox.convert_mbox in_chan converter)
end

module _ : CONVERT = Conversion_ocamlnet
