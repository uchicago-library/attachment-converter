module Type : sig
  type t

  val application : t
  val of_string : string -> (t, Error.t) result
  val pp : t Fmt.t
  val equal : t -> t -> bool
end

module Subtype : sig
  type t

  val pdf : t
  val plain : t
  val msword : t
  val docx : t
  val xls : t
  val xlsx : t
  val tsv : t
  val gif : t
  val bmp : t
  val tiff : t
  val jpeg : t
  val of_string : string -> (t, Error.t) result
  val pp : t Fmt.t
  val equal : t -> t -> bool
end

type t

val ty : t -> Type.t
val subty : t -> Subtype.t
val make : Type.t -> Subtype.t -> t
val to_string : t -> string
val of_string : string -> (t, Error.t) result
val extension : t -> string
val compare : t -> t -> int
val pp : t Fmt.t
val equal : t -> t -> bool
val pdf : t
val pdfa : t
val txt : t
val doc : t
val docx : t
val xls : t
val xlsx : t
val tsv : t
val gif : t
val tiff : t
val bmp : t
val jpeg : t
