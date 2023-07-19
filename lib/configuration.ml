open Prelude

module Formats = struct

  type transform_data =
    { target_type   : string;
      target_ext    : string;
      shell_command : string;
      convert_id    : string;
    }

    module Dict = Map.Make (String)

  type t = (transform_data list) Dict.t

  let mime_type_to_extension mt =
    match mt with
    | "application/pdf"           -> ".pdf"
    | "text/plain"                -> ".txt"
    | "text/tab-separated-values" -> ".tsv"
    | "image/tiff"                -> ".tif"
    | _ -> "" (* TODO: default to no extension should be logged *)

  let make =
    let make_t (ty, cm, id) =
      { target_type = ty
      ; target_ext = mime_type_to_extension ty
      ; shell_command = cm
      ; convert_id = id
      }
    in
    Dict.of_list Dict.empty << List.map (Pair.onright (List.map make_t))

  let conversions d ct =
    Option.default []
      (Dict.find_opt ct d)
 end

module ParseConfig = struct
  open Formats

  type config_key =
    | SourceType
    | TargetType
    | TargetExt
    | ShellCommand
    | ConvertID

  let string_of_config_key key =
    match key with
    | SourceType   -> "source_type"
    | TargetType   -> "target_type"
    | TargetExt    -> "target_extension"
    | ShellCommand -> "shell_command"
    | ConvertID    -> "id"

  module Error = struct
    type t = [
      | `ConfigData of int * config_key
      | `ReferParse of int * string
    ]

    let message err =
      match err with
      | `ConfigData (line_num, key)  ->
          Printf.sprintf
            "Config Data Error: (entry starting at line %d) Missing key '%s'"
            line_num
            (string_of_config_key key)
      | `ReferParse (line_num, line) ->
          Printf.sprintf
            "Refer Parse Error: (line %d) Cannot parse '%s'"
            line_num
            line
  end

  type config_entry =
    { source_type   : string ;
      target_type   : string ;
      target_ext    : string ;
      shell_command : string ;
      convert_id    : string ;
    }

  let entry_of_assoc config_assoc =
    let construct_config_entry st tt te ss id =
      { source_type   = st ;
        target_type   = tt ;
        target_ext    = te ;
        shell_command = ss ;
        convert_id    = id ;
      }
    in
    let check key =
      Option.to_result
        ~none:key
        (assoc_opt (string_of_config_key key) config_assoc)
    in
    let  ( let* ) = Result.(>>=)       in
    let* st       = check SourceType   in
    let* tt       = check TargetType   in
    let* ss       = check ShellCommand in
    let  te       = Result.default
                      (mime_type_to_extension tt)
                      (check TargetExt)
    in
    let* id       = check ConvertID    in
    Ok (construct_config_entry st tt te ss id)

  let transform_data_of_entry entry =
    { target_type   = entry.target_type   ;
      target_ext    = entry.target_ext    ;
      shell_command = entry.shell_command ;
      convert_id    = entry.convert_id    ;
    }

  let parse config_str =
    let (>>=) = Result.(>>=) in
    let insert_append k v =
      Dict.update k (fun curr -> Some (v :: Option.value curr ~default:[]))
    in
    let update_accum line_num next accum =
      Result.witherr (fun k -> `ConfigData (line_num, k)) (entry_of_assoc next) >>= (fun entry ->
        Ok (insert_append entry.source_type (transform_data_of_entry entry) accum))
    in
    let collect_varieties line_num next raccum = raccum >>= update_accum line_num next in
    let error_handler     line_num line rerror = rerror >>= (fun _ ->
      Error (`ReferParse (line_num, line)))
    in
    Refer.fold
      (Refer.witherr error_handler collect_varieties)
      (Ok Dict.empty)
      (Refer.of_string config_str)
end

let default_config () =
  let open Formats in
  let script_dir = File.squiggle "~/.config/attachment-converter/scripts/" in
  make
  [ "application/pdf" ,
    [ "application/pdf" , script_dir ^ "soffice-wrapper.sh -i pdf -o pdf" , "soffice-pdf-to-pdfa"
    ; "text/plain" , script_dir ^ "pdftotext-wrapper.sh" , "pdftotext-pdf-to-text"
    ]
  ; "application/msword" ,
    [ "application/pdf" , script_dir ^ "soffice-wrapper.sh -i doc -o pdf" , "soffice-doc-to-pdfa"
    ; "text/plain" , script_dir ^ "soffice-wrapper.sh -i doc -o txt" , "soffice-doc-to-txt"
    ]
  ; "application/vnd.openxmlformats-officedocument.wordprocessingml.document" ,
    [ "application/pdf" , script_dir ^ "soffice-wrapper.sh -i docx -o pdf" , "soffice-docx-to-pdfa"
    ; "text/plain" , script_dir ^ "pandoc-wrapper.sh -i docx -o txt" , "pandoc-docx-to-txt"
    ]
  ; "application/vnd.ms-excel" ,
    [ "text/tab-separated-values" , script_dir ^ "soffice-wrapper.sh -i xls -o tsv" , "soffice-xls-to-tsv" ]
  ; "image/gif" ,
    [ "image/tiff" , script_dir ^ "vips-wrapper.sh -i gif -o tif" , "vips-gif-to-tif" ]
  ;  "image/bmp" ,
    [ "image/tiff" , script_dir ^ "vips-wrapper.sh -i bmp -o tif" , "vips-bmp-to-tif" ]
  ;  "image/jpeg" ,
    [ "image/tiff" , script_dir ^ "vips-wrapper.sh -i bmp -o tif" , "vips-jpeg-to-tif" ]
  ]

let get_config config_files =
  let open ParseConfig in
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
  Option.(default (Ok (default_config ())) (map (readfile >> parse) config_opt))
