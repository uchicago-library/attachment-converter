open Prelude

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
    Ok (make
          ~source_type:st
          ~target_type:tt
          ~target_ext:te
          ~shell_command:sc
          ~convert_id:id)

  let to_transform_data ce =
    let ( let* ) = Result.(>>=) in
    let* target_mtype = Mime_type.of_string (target_type ce) in
    let ext = Mime_type.extension target_mtype in
    let td = Transform_data.make
      ~target_type:target_mtype
      ~target_ext:ext
      ~shell_command:(shell_command ce)
      ~convert_id:(convert_id ce)
    in
    Ok td
end

module Formats = struct
  module Dict = Map.Make (String)
  type t = (Transform_data.t list) Dict.t

  let make =
    let make_td (ty, cm, id) =
      Transform_data.make_no_ext
        ~target_type:ty
        ~shell_command:cm
        ~convert_id:id
    in
    Dict.of_list Dict.empty << List.map (Pair.onright (List.map make_td))

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
      let updated_dict = insert_append (source_type entry) trans_data accum in
      Ok updated_dict
    in
    let on_error line_num line _ = Error (`ReferParse (line_num, line)) in
    Refer.fold
      (Refer.witherr on_error on_ok)
      (Ok Dict.empty)
      (Refer.of_string config_str)

(*    let (>>=) = Result.(>>=) in
    let ( let* ) = Result.(>>=) in
    assert false
    let insert_append k v =
      Dict.update k (fun curr -> Some (v :: Option.value curr ~default:[]))
    in
    let update_accum line_num next accum =
      Result.witherr (fun k -> `ConfigData (line_num, k)) (Config_entry.of_refer next) >>= (fun entry ->
        Ok (insert_append (Config_entry.source_type entry) (Config_entry.to_transform_data entry) accum))
    in
    let collect_varieties line_num next raccum = raccum >>= update_accum line_num next in
    let error_handler     line_num line rerror = rerror >>= (fun _ ->
      Error (`ReferParse (line_num, line)))
    in
    Refer.fold
      (Refer.witherr error_handler collect_varieties)
      (Ok Dict.empty)
      (Refer.of_string config_str) *)
end

let default_config () =
  let open Formats in
  let script_dir = File.squiggle "~/.config/attachment-converter/scripts/" in
  let of_string_default x = Result.default (assert false) (Mime_type.of_string x) in
  make
  [ "application/pdf" ,
    [ of_string_default "application/pdf" , script_dir ^ "soffice-wrapper.sh -i pdf -o pdf" , "soffice-pdf-to-pdfa"
    ; of_string_default "text/plain" , script_dir ^ "pdftotext-wrapper.sh" , "pdftotext-pdf-to-text"
    ]
  ; "application/msword" ,
    [ of_string_default "application/pdf" , script_dir ^ "soffice-wrapper.sh -i doc -o pdf" , "soffice-doc-to-pdfa"
    ; of_string_default "text/plain" , script_dir ^ "soffice-wrapper.sh -i doc -o txt" , "soffice-doc-to-txt"
    ]
  ; "application/vnd.openxmlformats-officedocument.wordprocessingml.document" ,
    [ of_string_default "application/pdf" , script_dir ^ "soffice-wrapper.sh -i docx -o pdf" , "soffice-docx-to-pdfa"
    ; of_string_default "text/plain" , script_dir ^ "pandoc-wrapper.sh -i docx -o txt" , "pandoc-docx-to-txt"
    ]
  ; "application/vnd.ms-excel" ,
    [ of_string_default "text/tab-separated-values" , script_dir ^ "soffice-wrapper.sh -i xls -o tsv" , "soffice-xls-to-tsv" ]
  ; "image/gif" ,
    [ of_string_default "image/tiff" , script_dir ^ "vips-wrapper.sh -i gif -o tif" , "vips-gif-to-tif" ]
  ;  "image/bmp" ,
    [ of_string_default "image/tiff" , script_dir ^ "vips-wrapper.sh -i bmp -o tif" , "vips-bmp-to-tif" ]
  ;  "image/jpeg" ,
    [ of_string_default "image/tiff" , script_dir ^ "vips-wrapper.sh -i bmp -o tif" , "vips-jpeg-to-tif" ]
  ]

let get_config _ = assert false
  (* let config_files = *)
  (*   config_files @ *)
  (*   Option.to_list (Sys.getenv_opt "AC_CONFIG") @ *)
  (*   [ ~~ "~/.config/attachment-converter/acrc" ; ~~ "~/.acrc" ] *)
  (* in *)
  (* let config_opt = *)
  (*   config_files *)
  (*   |> List.dropwhile (not << Sys.file_exists) *)
  (*   |> List.head *)
  (* in *)
  (* let parsed = assert false in *)
  (* Option.(default (Ok (default_config ())) parsed) (\* (map (readfile >> parse) config_opt)) *\) *)
