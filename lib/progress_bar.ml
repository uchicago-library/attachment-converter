open Prelude

module Printer = struct
  let print msg =
    if Unix.isatty (Unix.descr_of_out_channel stderr)
    then prerr_endline msg
    else ()
end

let progress_barify base_func hush message_of arg =
  let echo_progress msg =
    if hush arg
    then ()
    else Printer.print msg
  in
  echo_progress (message_of arg) ;
  base_func arg
