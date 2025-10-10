exception Invalid_mbox

type t

type email =
  { from_line : string;
    from_line_num : int;
    after_from_line : string
  }

val of_in_channel : in_channel -> (t, Error.t) result
val of_in_channel_exn : in_channel -> t
val input_email : t -> email option
val close : t -> unit
val foldl : (email -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (email -> unit) -> t -> unit
