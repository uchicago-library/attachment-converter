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
      val of_string : string -> t option
      val to_string : t -> string
    end

    type t

    val value : t -> string
    val params : t -> Parameter.t list
    val make : ?params:Parameter.t list -> string -> t
    val of_string : string -> (t, Error.t) result
    val to_string : t -> string

    val lookup_param :
      ?quotes:bool -> string -> t -> string option

    val update_value : string -> t -> t

    val update_param :
      string ->
      (Parameter.t option -> Parameter.t option) ->
      t ->
      t

    val add_param :
      ?quotes:bool -> string -> string -> t -> t
  end

  type t

  val name : t -> string
  val value : t -> Value.t
  val to_assoc : t -> string * string
  val make : string -> Value.t -> t
  val of_string : string -> (t, Error.t) result
  val to_string : t -> string
end

type t

val to_list : t -> Field.t list
val of_list : Field.t list -> t

val of_assoc_list :
  (string * string) list -> (t, Error.t) result

val to_assoc_list : t -> (string * string) list
val to_string : t -> string

val update :
  (Field.Value.t option -> Field.Value.t option) ->
  string ->
  t ->
  t
