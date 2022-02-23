module Formats = struct

  type htransform = string -> string
  type dtransform = string -> string

  type variety =
    | DataOnly of dtransform
    | DataAndHeader of (htransform * dtransform)
    | NoChange

  module Dict = Map.Make (String)

  type t = (variety list) Dict.t

  module Error = struct
    type t =
      | ReferParse of string
      | ConfigData of string
      | Unix of (string * Unix.error)
      | MimeParse of string
      | CharacterEncoding of string

    let map_msg f err =
      match err with
      | ReferParse msg        -> ReferParse (f msg)
      | ConfigData msg        -> ConfigData (f msg)
      | Unix (msg, unix)      -> Unix (f msg, unix)
      | MimeParse msg         -> MimeParse (f msg)
      | CharacterEncoding msg -> CharacterEncoding (f msg)
  end
  type error = Error.t
end

module ParseConfig = struct
  open Prelude
  open Formats

  type config_entry =
    { source_type:  string;
      target_type:  string;
      shell_script: string }

  let config_entry_of_assoc config_assoc =
    let construct_config_entry st tt ss =
      { source_type = st;
        target_type = tt;
        shell_script = ss }
    in
    let check key err_msg =
      Option.to_result
        ~none:(Error.ConfigData ("Config File Error: " ^ err_msg))
        (assoc_opt key config_assoc)
    in
    let ( let* ) = Result.(>>=) in
    let* st = check "source_type" "no source type given" in
    let* tt = check "target_type" "no target type given" in
    let* ss = check "shell_script" "no script path given"
    in
    Ok (construct_config_entry st tt ss)

  let variety_of_config_entry entry =
    let header_transformer = id in (* update_mimetype entry.source_type entry.target_type in *)
    let data_transformer = id (* convert entry.shell_script *)
    in
    if entry.source_type = entry.target_type then
      DataOnly data_transformer
    else
      DataAndHeader (header_transformer, data_transformer)

  let parse_config_file (path_to_config: string) =
    let ( let* ) = Result.(>>=) in
    let open Result in
    let insert_append k v = Dict.update k (fun curr -> Some (v :: Option.value curr ~default:[])) in
    let config_str = Prelude.readfile path_to_config in
    let add_line_num i = Error.map_msg (Printf.sprintf "Line %d, %s" i) in
    let variety_dict_update line_num next accum =
        let* entry = config_entry_of_assoc next in
          Result.witherr (add_line_num line_num)
            (Ok (insert_append entry.source_type (variety_of_config_entry entry) accum))
    in
    let collect_varieties line_num next accum_or_error = accum_or_error >>= (variety_dict_update line_num next) in
    let error_handler line_num line accum_or_error = accum_or_error >>=
      (fun _ ->
        Error (Error.ReferParse (Printf.sprintf "Line %d, Refer Parse Error: Cannot parse '%s'" line_num line)))
    in
    Refer.fold
      (Refer.witherr error_handler collect_varieties)
      (Ok Dict.empty)
      (Refer.of_string config_str)

  

end
