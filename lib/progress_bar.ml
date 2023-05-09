module Printer = struct
  let print msg pbar =
    output_string pbar (msg ^ "\n") ;
    flush pbar
end
