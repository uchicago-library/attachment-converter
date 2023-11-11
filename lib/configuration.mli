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

  val make : target_type:Mime_type.t ->
             target_ext:string ->
             shell_command:string ->
             convert_id:string -> t

  val make_no_ext : target_type:Mime_type.t ->
                    shell_command:string ->
                    convert_id:string -> t

  val of_conv_util : ConvUtil.t -> Mime_type.t -> Mime_type.t -> t
end

module ConfigKey : sig
  type t = [
    | `SourceType
    | `TargetType
    | `TargetExt
    | `ShellCommand
    | `ConvertID
    ]

  val to_string : t -> string
end

module ConfigEntry : sig

  module Error : Utils.ERROR with
           type t = [
             | Mime_type.Error.t
             | `MissingKey of ConfigKey.t
             ]

  type t

  val source_type : t -> string
  val target_type : t -> string
  val target_ext : t -> string option
  val shell_command : t -> string
  val convert_id : t -> string

  val make : source_type:string ->
             target_type:string ->
             target_ext:string option ->
             shell_command:string ->
             convert_id:string -> t

  val to_refer : t -> Prelude.Refer.t
  val of_refer : Prelude.Refer.t -> (t, [> Error.t]) result

  val to_transform_data : t -> (TransformData.t, [> Error.t]) result
end

module Formats : sig
  type t

  val conversions : t -> Mime_type.t -> TransformData.t list

  module Error : Utils.ERROR with
           type t = [
             | `ConfigData of int * ConfigKey.t
             | `ReferParse of int * string
             | Mime_type.Error.t
             ]

  val of_assoc_list : (Mime_type.t * TransformData.t list) list -> t
  val of_string : string -> (t, [> Error.t]) result
end

val default_config : unit -> Formats.t
val get_config : string list -> (Formats.t, [> Formats.Error.t]) result
