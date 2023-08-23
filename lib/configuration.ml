open Prelude


module Conv_util = struct
  type t =
    { id : string ;
      envoke : Mime_type.t -> Mime_type.t -> string ;
    }

  let id ut = ut.id
  let envoke ut = ut.envoke

  let default_script_dir () = File.squiggle "~/.config/attachment-converter/scripts/"

  let script_call nm mt1 mt2 =
    let open Mime_type in
    default_script_dir () ^ nm ^ " -i " ^ extension mt1 ^ " -o " ^ extension mt2

  let soffice =
    { id = "soffice" ;
      envoke = fun mts mtt -> script_call "soffice-wrapper.sh" mts mtt ;
    }

  let pandoc =
    { id = "pandoc" ;
      envoke = fun mts mtt -> script_call "pandoc-wrapper.sh" mts mtt ;
    }

  let vips =
    { id = "vips" ;
      envoke = fun mts mtt -> script_call "vips-wrapper.sh" mts mtt ;
    }

  let pdftotext =
    { id = "pdftotext" ;
      envoke = fun _ _ -> default_script_dir () ^ "pdftotext-wapper.sh" ;
    }
end

module Transform_data = struct
  type t =
    { target_type : Mime_type.t ;
      target_ext : string ;
      shell_command : string ;
      convert_id : string ;
    }

  let target_type td = td.target_type
  let target_ext td = td.target_ext
  let shell_command td = td.shell_command
  let convert_id td = td.convert_id

  let make ~target_type ~target_ext ~shell_command ~convert_id =
    { target_type = target_type ;
      target_ext = target_ext ;
      shell_command = shell_command ;
      convert_id = convert_id ;
    }

  let make_no_ext ~target_type ~shell_command ~convert_id =
    make
      ~target_type:target_type
      ~target_ext:(Mime_type.extension target_type)
      ~shell_command:shell_command
      ~convert_id:convert_id

  let of_conv_util ut mti mto =
    let open Mime_type in
    let open Conv_util in
    make_no_ext
      ~target_type:mto
      ~shell_command:(envoke ut mti mto)
      ~convert_id:(id ut ^ "-" ^ extension mti ^ "-to-" ^ extension mto)
end

module Config_key = struct
  type t = [
    | `SourceType
    | `TargetType
    | `TargetExt
    | `ShellCommand
    | `ConvertID
    ]

  let to_string k =
    match k with
    | `SourceType -> "source_type"
    | `TargetType -> "target_type"
    | `TargetExt -> "target_ext"
    | `ShellCommand -> "shell_command"
    | `ConvertID -> "convert_id"
end

module Config_entry = struct

  module Error = Mime_type.Error

  type t =
    { source_type : string ;
      target_type : string ;
      target_ext : string option ;
      shell_command: string ;
      convert_id : string ;
    }

  let source_type ce = ce.source_type
  let target_type ce = ce.target_type
  let target_ext ce = ce.target_ext
  let shell_command ce = ce.shell_command
  let convert_id ce = ce.convert_id

  let make
        ~source_type
        ~target_type
        ~target_ext
        ~shell_command
        ~convert_id
    = { source_type = source_type ;
        target_type = target_type ;
        target_ext = target_ext ;
        shell_command = shell_command ;
        convert_id = convert_id ;
      }

  let to_refer entry =
    let open Config_key in
    Option.(to_list (map (fun ext -> "target_ext", ext) (target_ext entry)))
    @ [ to_string `SourceType , source_type entry ;
        to_string `TargetType , target_type entry ;
        to_string `ShellCommand , shell_command entry ;
        to_string `ConvertID , convert_id entry ;
      ]

  let of_refer rentry =
    let check key =
      Option.to_result
        ~none:key
        (assoc_opt (Config_key.to_string key) rentry)
    in
    let ( let* ) = Result.(>>=) in
    let* st = check `SourceType in
    let* tt = check `TargetType in
    let* sc = check `ShellCommand in
    let te = Result.to_option (check `TargetExt) in
    let* id = check `ConvertID in
    let entry =
      make
        ~source_type:st
        ~target_type:tt
        ~target_ext:te
        ~shell_command:sc
        ~convert_id:id
    in
    Ok entry

  let to_transform_data ce =
    let ( let* ) = Result.(>>=) in
    let* target_mtype = Mime_type.of_string (target_type ce) in
    let ext = Mime_type.extension target_mtype in
    let td =
      Transform_data.make
        ~target_type:target_mtype
        ~target_ext:ext
        ~shell_command:(shell_command ce)
        ~convert_id:(convert_id ce)
    in
    Ok td
end

module Formats = struct
  module Dict = Map.Make (Mime_type)
  type t = (Transform_data.t list) Dict.t

  let of_assoc_list =
    Dict.of_list Dict.empty

  let conversions d ct =
    Option.default []
      (Dict.find_opt ct d)

  module Error = struct
    type t = [
      | `ConfigData of int * Config_key.t
      | `ReferParse of int * string
      | `DummyError
      ]

    let message err =
      match err with
      | `ConfigData (line_num, key)  ->
         Printf.sprintf
           "Config Data Error: (entry starting at line %d) Missing key '%s'"
           line_num
           (Config_key.to_string key)
      | `ReferParse (line_num, line) ->
         Printf.sprintf
           "Refer Parse Error: (line %d) Cannot parse '%s'"
           line_num
           line
      | `DummyError -> "dummy"
  end

  let of_string config_str =
    let ( let* ) = Result.(>>=) in
    let insert_append k v =
      Dict.update k (fun curr -> Some (v :: Option.default [] curr))
    in
    let on_ok line_num next raccum =
      let open Config_entry in
      let* accum = raccum in
      let* entry= Result.witherr (fun k -> `ConfigData (line_num, k)) (of_refer next) in
      let* trans_data =
        Result.witherr (fun _ -> `DummyError) (to_transform_data entry) in
      let* mty = Result.witherr (fun _ -> `DummyError) (Mime_type.of_string (source_type entry)) in
      let updated_dict = insert_append mty trans_data accum in
      Ok updated_dict
    in
    let on_error line_num line _ = Error (`ReferParse (line_num, line)) in
    Refer.fold
      (Refer.witherr on_error on_ok)
      (Ok Dict.empty)
      (Refer.of_string config_str)
end

let default_config () =
  let open Mime_type in
  let open Formats in
  let open Conv_util in
  let into = flip << Transform_data.of_conv_util in
  let make source transformers = source , map ((|>) source) transformers in
  of_assoc_list
    [ make pdf [ into soffice pdfa ; into pdftotext txt ] ;
      make doc [ into soffice pdfa ; into soffice txt ] ;
      make docx [ into soffice pdfa ; into pandoc txt ] ;
      make xls  [ into soffice tsv ] ;
      make gif  [ into vips tiff ] ;
      make bmp  [ into vips tiff ] ;
      make jpeg [ into vips tiff ] ;
    ]

let get_config config_files =
  let config_files =
    config_files @
    Option.to_list (Sys.getenv_opt "AC_CONFIG") @
    [ ~~ "~/.config/attachment-converter/acrc" ; ~~ "~/.acrc" ]
  in
  let config_opt =
    config_files
    |> List.dropwhile (not << Sys.file_exists)
    |> List.head
  in
  Option.(default (Ok (default_config ())) (map (readfile >> Formats.of_string) config_opt))
