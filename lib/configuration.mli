module Formats :
  sig
    type transform_data = {
        target_type : string;
        target_ext : string;
        shell_command : string;
        convert_id : string;
      }
    type t

    module Dict :
      sig
        type key = string
        type 'a t
        val find : key -> 'a t -> 'a
      end

    val conversions : 'a list Dict.t ->
                      Dict.key ->
                      'a list
  end

module ParseConfig :
  sig
    type config_key =
        SourceType
      | TargetType
      | TargetExt
      | ShellCommand
      | ConvertID

    module Error :
      sig
        type t =
          [ `ConfigData of int * config_key
          | `ReferParse of int * string ]
        val message :
          [< `ConfigData of int * config_key
          | `ReferParse of int * string ] ->
          string
      end

    type config_entry = {
      source_type : string;
      target_type : string;
      target_ext : string;
      shell_command : string;
      convert_id : string;
    }

    val entry_of_assoc :
      (string * string) list ->
      (config_entry, config_key) result

    val transform_data_of_entry :
      config_entry ->
      Formats.transform_data

    val parse :
      string ->
      (Formats.transform_data list Formats.Dict.t,
       [> `ConfigData of int * config_key
       | `ReferParse of int * string ])
      result
  end

val get_config :
  string list ->
  (Formats.transform_data list Formats.Dict.t,
   [> `ConfigData of int * ParseConfig.config_key
    | `ReferParse of int * string ])
  result
