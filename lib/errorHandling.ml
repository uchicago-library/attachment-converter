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
    | `DummyError
    | `EmailParse of string
  ] = Convert.Conversion_ocamlnet.Error
