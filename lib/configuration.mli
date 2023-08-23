module Conv_util : sig
  type t

  val soffice : t
  val pandoc : t
  val vips : t
  val pdftotext : t
end

module Transform_data : sig
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

  val of_conv_util : Conv_util.t -> Mime_type.t -> Mime_type.t -> t
end

module Config_key : sig
  type t = [
    | `SourceType
    | `TargetType
    | `TargetExt
    | `ShellCommand
    | `ConvertID
    ]

  val to_string : t -> string
end

module Config_entry : sig

  module Error : Utils.ERROR

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
  val of_refer : Prelude.Refer.t -> (t, Config_key.t) result

  val to_transform_data : t -> (Transform_data.t, Error.t) result
end

module Formats : sig
  type t

  val conversions : t -> Mime_type.t -> Transform_data.t list

  module Error : Utils.ERROR with
           type t = [
             | `ConfigData of int * Config_key.t
             | `ReferParse of int * string
             | `DummyError
             ]

  val of_assoc_list : (Mime_type.t * Transform_data.t list) list -> t
  val of_string : string -> (t, Error.t) result
end

val default_config : unit -> Formats.t
val get_config : string list -> (Formats.t, Formats.Error.t) result
