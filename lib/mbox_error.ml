type t = [ `InvalidMBox ]

module Smart = struct
  let invalid_mbox = `InvalidMbox
end

let debug e =
  match e with
  | `InvalidMBox -> "first line of mbox is not a from line"
