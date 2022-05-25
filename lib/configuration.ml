open Prelude

module Formats = struct
  type variety =
    | DataOnly
    | DataAndHeader
    | NoChange

  type transform_data =
    { target_type   : string;
      target_ext    : string;
      shell_command : string;
      variety       : variety
    }

  module Dict = Map.Make (String)

  type t = (transform_data list) Dict.t

  let mime_type_to_extension mt =
    match mt with
    | "application/pdf" -> ".pdf"
    | "text/plain"      -> ".txt"
    | "text/tab-separated-values" -> ".tsv"
    | _ -> "" (* TODO: default to no extension should be logged *)
 end

module ParseConfig = struct
  open Formats

  type config_key =
    | SourceType
    | TargetType
    | TargetExt
    | ShellCommand

  let string_of_config_key key =
    match key with
    | SourceType   -> "source_type"
    | TargetType   -> "target_type"
    | TargetExt    -> "target_extension"
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
        | TargetExt    -> line_num + 2
        | ShellCommand -> line_num + 3
      in
      match err with
      | `ConfigData (line_num, key)  ->
          Printf.sprintf
            "Config Data Error: (line %d) Missing key '%s'"
            (config_error_line key line_num)
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
    }

  let entry_of_assoc config_assoc =
    let construct_config_entry st tt te ss =
      { source_type   = st ;
        target_type   = tt ;
        target_ext    = te ;
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
    let  te       = Result.default
                      (mime_type_to_extension tt)
                      (check TargetExt)
    in
    Ok (construct_config_entry st tt te ss)

  let transform_data_of_entry entry =
    { target_type   = entry.target_type   ;
      target_ext    = entry.target_ext    ;
      shell_command = entry.shell_command ;
      variety       = DataAndHeader       ;
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

  let parse_config_file = Prelude.readfile >> parse_config_str
end
