
(*
module type Error = sig
  type t = [> `dummy ]
end

module ErrorF (M : Error) (N : Error) : Error = struct
  type t = [ | M.t | N.t ]
end
*)

let message err =
  match err with
  | `DummyError     -> "Dummy error message"
  | `ReferParse msg -> msg
  | `ConfigData msg -> msg
