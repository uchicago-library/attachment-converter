type error_info =
  { date : string option;
    from : string option;
    message_id : string option
  }

type t =
  [ `MrmimeParseError of error_info
  | `OcamlnetParseError of error_info * string ]

module Smart = struct
  let ocamlnet_parse_error ~date ~from ~message_id msg =
    `OcamlnetParseError ({ date; from; message_id }, msg)

  let mrmime_parse_error ~date ~from ~message_id =
    `MrmimeParseError { date; from; message_id }
end
