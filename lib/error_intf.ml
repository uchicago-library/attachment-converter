type error =
  [ Config_entry_error.t
  | Mime_type_error.t
  | Formats_error.t
  | Mrmime_parsetree_error.t
  | Dependency_error.t ]

type t = error list

module type TRACE = sig
  type 'a trace

  val new_error : [< error] -> 'a trace
  val with_error : [< error] -> 'a trace -> 'a trace
end
