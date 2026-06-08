(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


module Type : sig
  type t

  val of_string : string -> (t, Error.t) result
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
end

type t

val ty : t -> Type.t
val subty : t -> Subtype.t
val make : Type.t -> Subtype.t -> t
val to_string : t -> string
val of_string : string -> (t, Error.t) result
val extension : t -> string
val compare : t -> t -> int
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
