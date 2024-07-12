type t =
  [ `ConfigData of int
  | `ReferParse of int * string ]

module Smart = struct
  let config_data_err line_num = `ConfigData line_num
  let refer_parse_err line_num not_parsed = `ReferParse (line_num, not_parsed)
end
