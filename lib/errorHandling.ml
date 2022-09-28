(* signature for errors as handled throughout this program *)
module type ERROR =
sig
  type t
  val message : t -> string
end

module _ : ERROR with
  type t = [
    | `ConfigData of int * Configuration.ParseConfig.config_key
    | `ReferParse of int * string
  ] = Configuration.ParseConfig.Error

module _ : ERROR with
  type t = [
    | `ParameterParse
  ] = Header.Field.Value.Error

module _ : ERROR with
  type t = [
    | `EmailParse
    | Header.Field.Value.Error.t
  ] = Convert.Converter.Error

module Error : ERROR with
  type t = [
    | Convert.Converter.Error.t
    | Configuration.ParseConfig.Error.t
  ]
  = struct
  type t = [
    | Convert.Converter.Error.t
    | Configuration.ParseConfig.Error.t
  ]

  let message err =
    match err with
    | #Convert.Converter.Error.t as e ->
        Convert.Converter.Error.message e
    | #Configuration.ParseConfig.Error.t as e ->
        Configuration.ParseConfig.Error.message e
end
