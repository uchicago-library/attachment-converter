(* signature for errors as handled throughout this program *)
module type ERROR =
sig
  type t
  val message : t -> string
end

module _ : ERROR with
  type t = [
    | `ReferParse of string
    | `ConfigData of string
  ] = Configuration.ParseConfig.Error

