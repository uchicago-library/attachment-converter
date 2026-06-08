(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


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


(* This file is part of Attachment Converter. *)

(* Attachment Converter is free software: you can redistribute it *)
(* and/or modify it under the terms of the GNU General Public License *)
(* as published by the Free Software Foundation, either version 3 of *)
(* the License, or (at your option) any later version. *)

(* Attachment Converter is distributed in the hope that it will be *)
(* useful, but WITHOUT ANY WARRANTY; without even the implied warranty *)
(* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Attachment Converter. If not, see *)
(* <https://www.gnu.org/licenses/>. *)
