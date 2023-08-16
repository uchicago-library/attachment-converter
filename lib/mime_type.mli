
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

type t

val type_of : t -> Type.t
val subtype : t -> Subtype.t

val to_string : t -> string
val of_string : string -> t

val extension : t -> string
