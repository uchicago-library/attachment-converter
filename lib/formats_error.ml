type t = [`ConfigData of int | `ReferParse of int * string]

module Smart = struct
  let config_data_err line_num = `ConfigData line_num

  let refer_parse_err line_num not_parsed =
    `ReferParse (line_num, not_parsed)
end

let debug e =
  match e with
  | `ConfigData _ -> "Bad config data"
  | `ReferParse (_, line) ->
    "refer parse error at line: " ^ line
