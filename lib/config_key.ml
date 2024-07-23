type t =
  [ `SourceType
  | `TargetType
  | `TargetExt
  | `ShellCommand
  | `ConvertID ]

let to_string k =
  match k with
  | `SourceType -> "source_type"
  | `TargetType -> "target_type"
  | `TargetExt -> "target_ext"
  | `ShellCommand -> "shell_command"
  | `ConvertID -> "id"
