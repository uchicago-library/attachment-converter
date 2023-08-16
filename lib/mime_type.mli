
module Type : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
end

module Subtype : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
end

module Error : Utils.ERROR

type t

val type_of : t -> Type.t
val subtype : t -> Subtype.t

val to_string : t -> string
val of_string : string -> (t, Error.t) result

val extension : t -> string
