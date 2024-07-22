type t =
  [ `SourceType
  | `TargetType
  | `TargetExt
  | `ShellCommand
  | `ConvertID ]

val to_string : t -> string
