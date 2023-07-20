module Error : sig
  (** test docstring  *)
  type t =
    [ `ConfigData of
        int * Lib__Configuration.ParseConfig.config_key
    | `EmailParse
    | `ReferParse of int * string ]
  (** test more docstring  *)
  val message : t -> string
end

module Printer : sig
  val print : string -> unit
end
