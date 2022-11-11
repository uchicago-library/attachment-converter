open OUnit2
open Utils
open Lib.Header.Field.Value

let basic_cont_dis =
"attachment;    filename*=utf-8''test.gif;    filename=\"test.gif\""

let basic_cont_dis_conv_0 =
"attachment;    filename*=utf-8''test_CONVERTED.tiff;    filename=\"test.gif\""

let basic_cont_dis_conv_1 =
"attachment;    filename*=utf-8''test.gif;    filename=\"test_CONVERTED.tiff\""

let basic_cont_dis_conv_2 =
"attachment;    filename*=utf-8''test_CONVERTED.tiff;    filename=\"test_CONVERTED.tiff\""

let basic_cont_dis_1 =
"attachment;    filename=\"test.gif\";    filename*=utf-8''test.gif"

let basic_cont_dis_conv_3 =
"attachment;    filename=\"test_CONVERTED.tiff\";    filename*=utf-8''test_CONVERTED.tiff"

let of_string_test1 =
  let param1 : Parameter.t = { attr = "filename*"; value = "utf-8''test.gif"; quotes = false } in
  let param2 : Parameter.t = { attr = "filename"; value = "test.gif"; quotes = true } in
  check_eq_basic
    "parsing basic_cont_dis"
    (Ok { value = "attachment"; params = [param1 ; param2 ] })
    (of_string basic_cont_dis)

let cd = Result.get_ok (of_string basic_cont_dis)

let to_string_test1 =
  check_eq_string
    "basic to_string of basic_cont_dis test"
    "attachment;\n\tfilename*=utf-8''test.gif;\n\tfilename=\"test.gif\""
    (to_string cd)

let to_string_test2 =
  check_eq_string
    "basic to_string without params"
    "attachment"
    (to_string (Result.get_ok (of_string "attachment")))

let lookup_param_test1 =
  check_eq_basic
    "basic lookup_param test"
    (Some "\"test.gif\"")
    (lookup_param "filename" cd)

let update_test1 =
  let param1: Parameter.t = { attr = "filename*"; value = "utf-8''test.gif"; quotes = false } in
  let param2: Parameter.t = { attr = "filename"; value = "test.gif"; quotes = true } in
    check_eq_basic
      "simple update_value test"
      { value = "inline"; params = [param1 ; param2 ] }
      (update_value "inline" cd)

let tests =
  "test suite for header field values" >:::
    [ of_string_test1 ;
      to_string_test1 ;
      to_string_test2 ;
      update_test1 ;
      lookup_param_test1 ;
    ]

let _ = run_test_tt_main tests
