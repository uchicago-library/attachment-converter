type error = Configuration_error.t

type t = error list

module type TRACE = sig
  type 'a trace

  val new_error : [< error] -> 'a trace
  val with_error : [< error] -> 'a trace -> 'a trace
end
