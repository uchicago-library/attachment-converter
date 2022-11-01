open Prelude

module Printer = struct
  let print msg = if Unix.isatty (Unix.descr_of_out_channel stderr)
                  then prerr_endline msg
                  else ()
end
