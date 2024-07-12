(* module ConfigKey : sig *)
(*   type t = *)
(*     [ `SourceType *)
(*     | `TargetType *)
(*     | `TargetExt *)
(*     | `ShellCommand *)
(*     | `ConvertID ] *)

(*   val to_string : t -> string *)
(* end *)

module Config_entry : sig
  (* module Error : *)
  (*   Utils.ERROR *)
  (*     with type t = *)
  (*       [ `MissingKey of Config_key.t *)
  (*       | `BadMimeType of string * Config_key.t ] *)

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

  val of_refer :
    Prelude.Refer.t -> (t, Error.t) result
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

val get_config :
  string list -> (Formats.t, Error.t) result
