open Prelude

module Formats = struct
  type variety =
    | DataOnly
    | DataAndHeader
    | NoChange

  type transform_data =
    { target_type   : string;
      shell_command : string;
      variety       : variety
    }

  module Dict = Map.Make (String)

  type t = (transform_data list) Dict.t
end

module ParseConfig = struct
  open Formats

  type config_key =
    | SourceType
    | TargetType
    | ShellCommand

  let string_of_config_key key =
    match key with
    | SourceType   -> "source_type"
    | TargetType   -> "target_type"
    | ShellCommand -> "shell_command"

  module Error = struct
    type t =
      [ | `ConfigData of int * config_key
        | `ReferParse of int * string
      ]

    let message err =
      let config_error_line key line_num =
        match key with
        | SourceType   -> line_num
        | TargetType   -> line_num + 1
        | ShellCommand -> line_num + 2
      in
      match err with
      | `ReferParse (line_num, key)  -> "TODO"
      | `ConfigData (line_num, line) -> "TODO"
  end

  type config_entry =
    { source_type   : string ;
      target_type   : string ;
      shell_command : string ;
    }

  let entry_of_assoc config_assoc =
    let construct_config_entry st tt ss =
      { source_type   = st ;
        target_type   = tt ;
        shell_command = ss ;
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
    Ok (construct_config_entry st tt ss)

  let transform_data_of_entry entry =
    let variety_of_entry =
      if   entry.source_type = entry.target_type
      then DataOnly
      else DataAndHeader
    in
    { target_type   = entry.target_type   ;
      shell_command = entry.shell_command ;
      variety       = variety_of_entry    ;
    }

  let parse_config_str config_str =
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

  let parse_config_file path_to_config =
    path_to_config |> Prelude.readfile |> parse_config_str
end
