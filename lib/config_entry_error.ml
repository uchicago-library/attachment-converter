type t =
  [ `MissingKey of Config_key.t
  | `BadMimeType of string * Config_key.t ]

module Smart = struct
  let missing_key_err k = `MissingKey k
  let bad_mime_err ty k = `BadMimeType (ty, k)
end
