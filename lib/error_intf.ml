type error =
  [ Config_entry_error.t
  | Mime_type_error.t
  | Formats_error.t
  | Ocamlnet_parsetree_error.t
  | Mrmime_parsetree_error.t
  | Dependency_error.t ]

type t = error list

module type TRACE = sig
  val new_error : [< error] -> ('a, t) result

  val with_error :
    [< error] -> ('a, t) result -> ('a, t) result
end
