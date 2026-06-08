(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


module Config_entry : sig
  type t

  val source_type : t -> Mime_type.t
  val target_type : t -> Mime_type.t
  val target_ext : t -> string
  val shell_command : t -> string
  val convert_id : t -> string

  val make :
    source_type:Mime_type.t ->
    target_type:Mime_type.t ->
    target_ext:string option ->
    shell_command:string ->
    convert_id:string ->
    t

  val to_refer : t -> Prelude.Refer.t
  val of_refer : Prelude.Refer.t -> (t, Error.t) result
end

module ConvUtil : sig
  type t

  val soffice : t
  val pandoc : t
  val vips : t
  val pdftotext : t
end

module TransformData : sig
  type t

  val target_type : t -> Mime_type.t
  val target_ext : t -> string
  val shell_command : t -> string
  val convert_id : t -> string
  val debug : t -> string

  val make :
    target_type:Mime_type.t ->
    target_ext:string ->
    shell_command:string ->
    convert_id:string ->
    t

  val make_no_ext :
    target_type:Mime_type.t ->
    shell_command:string ->
    convert_id:string ->
    t

  val of_config_entry :
    Config_entry.t -> (t, Error.t) result

  val of_conv_util :
    ConvUtil.t -> Mime_type.t -> Mime_type.t -> t
end

module Formats : sig
  type t

  val conversions : t -> Mime_type.t -> TransformData.t list

  val of_assoc_list :
    (Mime_type.t * TransformData.t list) list -> t

  val of_string : string -> (t, Error.t) result
end

val default_config : unit -> Formats.t
val get_config : string list -> (Formats.t, Error.t) result


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
