open OUnit2
open Utils
module H = Lib.Header
module F = H.Field
module V = F.Value
module P = V.Parameter

let basic_cont_dis =
  "attachment;\r\n\
  \ \tfilename*=utf-8''test.gif;\r\n\
  \ \tfilename=\"test.gif\""

let of_string_test1 =
  let param1 = P.make "filename*" "utf-8''test.gif" in
  let param2 = P.make ~quotes:true "filename" "test.gif" in
  check_eq_basic "basic of_string of basic_cont_dis test"
    (Ok (V.make ~params:[ param1; param2 ] "attachment"))
    (V.of_string basic_cont_dis)

let cd = Result.get_ok (V.of_string basic_cont_dis)

let to_string_test1 =
  check_eq_string "basic to_string of basic_cont_dis test"
    basic_cont_dis (V.to_string cd)

let to_string_test2 =
  check_eq_string "basic to_string without params"
    "attachment"
    (V.to_string (Result.get_ok (V.of_string "attachment")))

let lookup_param_test1 =
  check_eq_basic "basic lookup_param test" (Some "test.gif")
    (V.lookup_param "filename" cd)

let update_tests =
  let param1 = P.make "filename*" "utf-8''test.gif" in
  let param2 = P.make ~quotes:true "filename" "test.gif" in
  let param3 = P.make "filename" "ok.gif" in
  let param4 = P.make ~quotes:true "whatever" "ok.gif" in
  let test1 =
    check_eq_basic "basic update_value test"
      (V.make ~params:[ param1; param2 ] "inline")
      (V.update_value "inline" cd)
  in
  let test2 =
    check_eq_basic "basic update_param test"
      (V.make ~params:[ param1; param3 ] (V.value cd))
      (V.update_param "filename" (fun _ -> Some param3) cd)
  in
  let test3 =
    check_eq_basic "basic update_param remove test"
      (V.make ~params:[ param1 ] (V.value cd))
      (V.update_param "filename" (fun _ -> None) cd)
  in
  let test4 =
    check_eq_basic "basic update_param no op test" cd
      (V.update_param "file" (fun _ -> None) cd)
  in
  let test5 =
    check_eq_basic "basic add_param test"
      (V.make
         ~params:[ param1; param2; param4 ]
         (V.value cd) )
      (V.add_param ~quotes:true "whatever" "ok.gif" cd)
  in
  let test6 =
    check_eq_basic "basic add_param with quotes test"
      (V.make
         ~params:
           [ param1;
             param2;
             P.make ~quotes:true (P.attr param4)
               (P.value param4)
           ]
         (V.value cd) )
      (V.add_param ~quotes:true "whatever" "ok.gif" cd)
  in
  let test7 =
    check_eq_basic "add_param as update test"
      (V.make ~params:[ param1; param3 ] (V.value cd))
      (V.add_param "filename" "ok.gif" cd)
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
