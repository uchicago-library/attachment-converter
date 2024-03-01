open Prelude

module ConfigKey = struct
  type t =
    [ `SourceType
    | `TargetType
    | `TargetExt
    | `ShellCommand
    | `ConvertID ]

  let to_string k =
    match k with
    | `SourceType -> "source_type"
    | `TargetType -> "target_type"
    | `TargetExt -> "target_ext"
    | `ShellCommand -> "shell_command"
    | `ConvertID -> "id"
end

module ConfigEntry = struct
  module Error = struct
    type t =
      [ `MissingKey of ConfigKey.t
      | `BadMimeType of string * ConfigKey.t ]

    let message err =
      match err with
      | `MissingKey key ->
        Printf.sprintf
          "Config Entry Error: Missing key '%s'"
          (ConfigKey.to_string key)
      | `BadMimeType (given_str, key) ->
        Printf.sprintf
          "Bad Mime Type Error: Ill-formed mime type '%s' \
           given for '%s'"
          given_str
          (ConfigKey.to_string key)
  end

  type t =
    { source_type : Mime_type.t;
      target_type : Mime_type.t;
      target_ext : string;
      shell_command : string;
      convert_id : string
    }

  let source_type ce = ce.source_type
  let target_type ce = ce.target_type
  let target_ext ce = ce.target_ext
  let shell_command ce = ce.shell_command
  let convert_id ce = ce.convert_id

  let make ~source_type ~target_type ~target_ext
      ~shell_command ~convert_id =
    { source_type;
      target_type;
      target_ext =
        Option.default
          (Mime_type.extension target_type)
          target_ext;
      shell_command;
      convert_id
    }

  let to_refer entry =
    let open ConfigKey in
    [ ( to_string `SourceType,
        Mime_type.to_string (source_type entry) );
      ( to_string `TargetType,
        Mime_type.to_string (target_type entry) );
      (to_string `TargetExt, target_ext entry);
      (to_string `ShellCommand, shell_command entry);
      (to_string `ConvertID, convert_id entry)
    ]

  let of_refer rentry =
    let check key =
      Option.to_result ~none:(`MissingKey key)
        (assoc_opt (ConfigKey.to_string key) rentry)
    in
    let ( let* ) = Result.( >>= ) in
    let* st_str = check `SourceType in
    let* st =
      Result.witherr
        (k (`BadMimeType (st_str, `SourceType)))
        (Mime_type.of_string st_str)
    in
    let* tt_str = check `TargetType in
    let* tt =
      Result.witherr
        (k (`BadMimeType (tt_str, `TargetType)))
        (Mime_type.of_string tt_str)
    in
    let* sc = check `ShellCommand in
    let te = Result.to_option (check `TargetExt) in
    let* id = check `ConvertID in
    let entry =
      make ~source_type:st ~target_type:tt ~target_ext:te
        ~shell_command:sc ~convert_id:id
    in
    Ok entry
end

module ConvUtil = struct
  type t =
    { identifier : string;
      envoke : Mime_type.t -> Mime_type.t -> string
    }

  let identifier ut = ut.identifier
  let envoke ut = ut.envoke

  let default_script_dir () =
    File.squiggle "~/.config/attachment-converter/scripts/"

  let script_call nm mt1 mt2 =
    let open Mime_type in
    String.(
      concat ""
        [ default_script_dir ();
          nm;
          " -i ";
          trimleft "." (extension mt1);
          " -o ";
          trimleft "." (extension mt2)
        ] )

  let soffice =
    { identifier = "soffice";
      envoke =
        (fun mts mtt ->
          script_call "soffice-wrapper.sh" mts mtt )
    }

  let pandoc =
    { identifier = "pandoc";
      envoke =
        (fun mts mtt ->
          script_call "pandoc-wrapper.sh" mts mtt )
    }

  let vips =
    { identifier = "vips";
      envoke =
        (fun mts mtt ->
          script_call "vips-wrapper.sh" mts mtt )
    }

  let pdftotext =
    { identifier = "pdftotext";
      envoke =
        (fun _ _ ->
          default_script_dir () ^ "pdftotext-wapper.sh" )
    }
end

module TransformData = struct
  type t =
    { target_type : Mime_type.t;
      target_ext : string;
      shell_command : string;
      convert_id : string
    }

  let target_type td = td.target_type
  let target_ext td = td.target_ext
  let shell_command td = td.shell_command
  let convert_id td = td.convert_id

  let make ~target_type ~target_ext ~shell_command
      ~convert_id =
    { target_type; target_ext; shell_command; convert_id }

  let make_no_ext ~target_type ~shell_command ~convert_id =
    make ~target_type
      ~target_ext:(Mime_type.extension target_type)
      ~shell_command ~convert_id

  let of_config_entry ce =
    let td =
      make
        ~target_type:(ConfigEntry.target_type ce)
        ~target_ext:(ConfigEntry.target_ext ce)
        ~shell_command:(ConfigEntry.shell_command ce)
        ~convert_id:(ConfigEntry.convert_id ce)
    in
    Ok td

  let of_conv_util ut mti mto =
    let open Mime_type in
    let open ConvUtil in
    let conv_id =
      String.(
        concat ""
          [ identifier ut;
            "-";
            trimleft "." (extension mti);
            "-to-";
            trimleft "." (extension mto)
          ] )
    in
    make_no_ext ~target_type:mto
      ~shell_command:(envoke ut mti mto) ~convert_id:conv_id
end

module Formats = struct
  module Dict = Map.Make (Mime_type)

  type t = TransformData.t list Dict.t

  let of_assoc_list = Dict.of_list Dict.empty

  let conversions d ct =
    Option.default [] (Dict.find_opt ct d)

  module Error = struct
    type t =
      [ `ConfigData of int * ConfigKey.t
      | `ReferParse of int * string
      | `BadMimeType of string * ConfigKey.t * int ]

    let message err =
      match err with
      | `ConfigData (line_num, key) ->
        Printf.sprintf
          "Config Data Error: (entry starting at line %d) \
           Missing key '%s'"
          line_num
          (ConfigKey.to_string key)
      | `ReferParse (line_num, line) ->
        Printf.sprintf
          "Refer Parse Error: (line %d) Cannot parse '%s'"
          line_num line
      | `BadMimeType (badmime, key, line_num) ->
        Printf.sprintf
          "Bad Mime Type Error: (line %d) Ill-formed mime \
           type '%s' at given for '%s'"
          line_num badmime
          (ConfigKey.to_string key)
  end

  let of_string config_str =
    let ( let* ) = Result.( >>= ) in
    let insert_append k v =
      Dict.update k (fun curr ->
          Some (v :: Option.default [] curr) )
    in
    let on_ok line_num next raccum =
      let open ConfigEntry in
      let* accum = raccum in
      let convert_error err =
        match err with
        | `MissingKey key -> `ConfigData (line_num, key)
        | `BadMimeType (badmime, key) ->
          `BadMimeType (badmime, key, line_num)
      in
      let* entry =
        Result.witherr convert_error (of_refer next)
      in
      let* trans_data =
        TransformData.of_config_entry entry
      in
      let mty = source_type entry in
      let updated_dict =
        insert_append mty trans_data accum
      in
      Ok updated_dict
    in
    let on_error line_num line _ =
      Error (`ReferParse (line_num, line))
    in
    Refer.fold
      (Refer.witherr on_error on_ok)
      (Ok Dict.empty)
      (Refer.of_string config_str)
end

let into = flip << TransformData.of_conv_util

let conv source transformers =
  (source, map (( |> ) source) transformers)

let default_assoc_list () =
  let open ConvUtil in
  let open Mime_type in
  [ conv pdf [ into soffice pdfa; into pdftotext txt ];
    conv doc [ into soffice pdfa; into soffice txt ];
    conv docx [ into soffice pdfa; into pandoc txt ];
    conv xls [ into soffice tsv ];
    conv gif [ into vips tiff ];
    conv bmp [ into vips tiff ];
    conv jpeg [ into vips tiff ]
  ]

let default_config () =
  Formats.of_assoc_list (default_assoc_list ())

let get_config config_files =
  let config_files =
    config_files
    @ Option.to_list (Sys.getenv_opt "AC_CONFIG")
    @ [ ~~"~/.config/attachment-converter/acrc";
        ~~"~/.acrc"
      ]
  in
  let config_opt =
    config_files
    |> List.dropwhile (not << Sys.file_exists)
    |> List.head
  in
  Option.(
    default
      (Ok (default_config ()))
      (map (readfile >> Formats.of_string) config_opt) )
