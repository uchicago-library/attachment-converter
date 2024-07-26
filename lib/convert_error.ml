type error_info =
  { date : string option; from : string option }

type t =
  [ `MrmimeParseError of error_info
  | `OcamlnetParseError of error_info * string ]

module Smart = struct
  let ocamlnet_parse_error ?date ?from msg =
    `OcamlnetParseError ({ date; from }, msg)

  let mrmime_parse_error ?date ?from () =
    `MrmimeParseError { date; from }
end
