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
    type t =
      | ReferParse of string
      | ConfigData of string

    let map_msg f err =
      match err with
      | ReferParse msg -> ReferParse (f msg)
      | ConfigData msg -> ConfigData (f msg)
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
        ~none:(Error.ConfigData ("Config File Error: " ^ err_msg))
        (assoc_opt key config_assoc)
    in
    let  ( let* ) = Result.(>>=)                                 in
    let* st       = check "source_type"   "no source type given" in
    let* tt       = check "target_type"   "no target type given" in
    let* ss       = check "shell_command" "no script path given"
    in
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
      Error (Error.ReferParse
        (Printf.sprintf "Line %d, Refer Parse Error: Cannot parse '%s'" line_num line)))
    in
    Refer.fold
      (Refer.witherr error_handler collect_varieties)
      (Ok Dict.empty)
      (Refer.of_string config_str)

  let parse_config_file path_to_config =
    path_to_config |> Prelude.readfile |> parse_config_str

  let default_config_str =
"%source_type application/pdf
%target_type application/pdf
%shell_command ./conversion-scripts/soffice-wrapper -i pdf -o pdf

%source_type application/pdf
%target_type text/plain
%shell_command ./conversion-scripts/pdftotext-wrapper.sh

%source_type application/msword
%target_type application/pdf
%shell_command ./conversion-scripts/soffice-wrapper.sh -i doc -o pdf

%source_type application/msword
%target_type text/plain
%shell_command ./conversion-scripts/soffice-wrapper.sh -i doc -o txt

%source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
%target_type application/pdf
%shell_command ./conversion-scripts/soffice-wrapper.sh -i docx -o pdf

%source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
%target_type text/plain
%shell_command ./conversion-scripts/pandoc-wrapper.sh -i docx -o txt

%source_type application/vnd.ms-excel
%target_type text/tab-separated-values
%shell_command ./conversion-scripts/soffice-wrapper.sh -i xls -o tsv

%source_type image/gif
%target_type image/tiff
%shell_command ./conversion-scripts/vips-wrapper.sh -i gif -o tif

%source_type image/bmp
%target_type image/tiff
%shell_command ./conversion-scripts/vips-wapper.sh -i bmp -o tif

%source_type image/jpeg
%target_type image/tiff
%shell_command ./conversion-scripts/vips-wrapper.sh -i jpeg -o tif"

  let default_config_fn = "default-config"

  let create_default_config _ =
    if   not (Sys.file_exists default_config_fn)
    then Prelude.writefile ~fn:default_config_fn default_config_str
    else ()

  let default_config () =
    create_default_config ();
    parse_config_file default_config_fn
end
