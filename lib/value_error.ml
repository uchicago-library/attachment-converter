type t = [`ParameterParse | `ValueParse]

module Smart = struct
  let param_parse_err = `ParameterParse
  let value_parse_err = `ValueParse
end
