open OUnit2
open Utils
open Lib.Header.Field.Value

let basic_cont_dis =
  "attachment;\r\n\
   \tfilename*=utf-8''test.gif;\r\n\
   \tfilename=\"test.gif\""

let of_string_test1 =
  let param1 : Parameter.t =
    { attr = "filename*";
      value = "utf-8''test.gif";
      quotes = false
    }
  in
  let param2 : Parameter.t =
    { attr = "filename"; value = "test.gif"; quotes = true }
  in
  check_eq_basic "basic of_string of basic_cont_dis test"
    (Ok
       { value = "attachment"; params = [ param1; param2 ] }
    )
    (of_string basic_cont_dis)

let cd = Result.get_ok (of_string basic_cont_dis)

let to_string_test1 =
  check_eq_string "basic to_string of basic_cont_dis test"
    basic_cont_dis (to_string cd)

let to_string_test2 =
  check_eq_string "basic to_string without params"
    "attachment"
    (to_string (Result.get_ok (of_string "attachment")))

let lookup_param_test1 =
  check_eq_basic "basic lookup_param test"
    (Some "\"test.gif\"")
    (lookup_param "filename" cd)

let update_tests =
  let param1 : Parameter.t =
    { attr = "filename*";
      value = "utf-8''test.gif";
      quotes = false
    }
  in
  let param2 : Parameter.t =
    { attr = "filename"; value = "test.gif"; quotes = true }
  in
  let param3 : Parameter.t =
    { attr = "filename"; value = "ok.gif"; quotes = false }
  in
  let param4 : Parameter.t =
    { attr = "whatever"; value = "ok.gif"; quotes = false }
  in
  let test1 =
    check_eq_basic "basic update_value test"
      { value = "inline"; params = [ param1; param2 ] }
      (update_value "inline" cd)
  in
  let test2 =
    check_eq_basic "basic update_param test"
      { cd with params = [ param1; param3 ] }
      (update_param "filename" (fun _ -> Some param3) cd)
  in
  let test3 =
    check_eq_basic "basic update_param remove test"
      { cd with params = [ param1 ] }
      (update_param "filename" (fun _ -> None) cd)
  in
  let test4 =
    check_eq_basic "basic update_param no op test" cd
      (update_param "file" (fun _ -> None) cd)
  in
  let test5 =
    check_eq_basic "basic add_param test"
      { cd with params = [ param1; param2; param4 ] }
      (add_param "whatever" "ok.gif" cd)
  in
  let test6 =
    check_eq_basic "basic add_param with quotes test"
      { cd with
        params =
          [ param1; param2; { param4 with quotes = true } ]
      }
      (add_param ~quotes:true "whatever" "ok.gif" cd)
  in
  let test7 =
    check_eq_basic "add_param as update test"
      { cd with params = [ param1; param3 ] }
      (add_param "filename" "ok.gif" cd)
  in
  "all update tests"
  >::: [ test1; test2; test3; test4; test5; test6; test7 ]

let tests =
  "test suite for header field values"
  >::: [ of_string_test1;
         to_string_test1;
         to_string_test2;
         update_tests;
         lookup_param_test1
       ]

let _ = run_test_tt_main tests
