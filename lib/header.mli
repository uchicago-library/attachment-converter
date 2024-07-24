module Field : sig
  module Value : sig
    module Parameter : sig
      type t

      val attr : t -> string
      val value : ?quotes:bool -> t -> string
      val quotes : t -> bool
      val make : ?quotes:bool -> string -> string -> t
      val map_attr : (string -> string) -> t -> t
      val map_value : (string -> string) -> t -> t
      val of_string : string -> t
      val to_string : t -> string
    end

    type t

    val value : t -> string
    val params : t -> Parameter.t list
    val make : ?params:Parameter.t list -> string -> t
    val of_string : string -> t
    val to_string : t -> string

    val lookup_param :
      ?quotes:bool -> string -> t -> string option
  end

  type t

  val name : t -> string
  val value : t -> Value.t
  val to_assoc : t -> string * string
  val make : string -> Value.t -> t
  val of_string : string -> t
  val to_string : t -> string
end

type t

val of_list : Field.t list -> t
val to_assoc_list : t -> (string * string) list
val to_string : t -> string
