type t =
  [ `MissingKey of Config_key.t
  | `BadMimeType of string * Config_key.t ]
