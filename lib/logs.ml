
module type LOG = sig
  type init
  type t
  val opens : init -> t
  val close : t -> unit
  val write : t -> string -> unit
end

module StringLog = struct
  type init = unit

  let empty : init = ()

  type t = { mutable curr : string }

  let opens () = { curr = "" }
  let close _ = ()
  let write = (^)
end

module OutChannelLog = struct
  type init =
    | StdOut
    | StdErr
    | File of string

  let std_out   : init = StdOut
  let std_err   : init = StdErr
  let file name : init = File name

  type t = out_channel

  let opens c =
    match c with
    | StdOut -> stdout
    | StdErr -> stderr
    | File name -> open_out name

  let close c = close_out c

  let write = output_string
end

module _ : LOG = OutChannelLog
