open Prelude

module Formats = struct
  type variety =
    | DataOnly
    | DataAndHeader
    | NoChange

  type transform_data =
    { target_type: string;
      shell_command: string;
      variety: variety
    }

  module Dict = Map.Make (String)

  type t = (transform_data list) Dict.t

  module Error = struct
    type t = [
      | `ReferParse of string
      | `ConfigData of string
    ]

    let map_msg f err =
      match err with
      | `ReferParse msg -> `ReferParse (f msg)
      | `ConfigData msg -> `ConfigData (f msg)
  end

  type error = Error.t
end

module ParseConfig = struct
  open Formats

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
    let check key err_msg =
      Option.to_result
        ~none:(`ConfigData ("Config File Error: " ^ err_msg))
        (assoc_opt key config_assoc)
    in
    let  ( let* ) = Result.(>>=)                                 in
    let* st       = check "source_type"   "no source type given" in
    let* tt       = check "target_type"   "no target type given" in
    let* ss       = check "shell_command" "no script path given" in
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
    let add_line_num i =
      Error.map_msg (fun msg -> Printf.sprintf "Entry starting at Line %d, %s" i msg)
    in
    let update_accum line_num next accum =
      Result.witherr (add_line_num line_num) (entry_of_assoc next) >>= (fun entry ->
        Ok (insert_append entry.source_type (transform_data_of_entry entry) accum))
    in
    let collect_varieties line_num next raccum = raccum >>= update_accum line_num next in
    let error_handler line_num line rerror     = rerror >>= (fun _ ->
      Error (`ReferParse
        (Printf.sprintf "Line %d, Refer Parse Error: Cannot parse '%s'" line_num line)))
    in
    Refer.fold
      (Refer.witherr error_handler collect_varieties)
      (Ok Dict.empty)
      (Refer.of_string config_str)

  let parse_config_file path_to_config =
    path_to_config |> Prelude.readfile |> parse_config_str
end
