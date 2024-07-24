type t = [`MimeParseError]

module Smart = struct
  let parse_err = `MimeParseError
end

let debug e =
  match e with
  | `MimeParseError -> "Mime Type Error: parse error"
