type error = Error_intf.error
type t = Error_intf.t

module T : sig
  include Error_intf.TRACE

  val new_list : [< error] -> t
  val throw : [< error] -> ('a, t) result
  val of_option : [< error] -> 'a option -> ('a, t) result

  val of_result :
    [< error] -> ('a, 'e) result -> ('a, t) result
end
