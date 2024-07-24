open OUnit2
open Utils
open Prelude
module H = Lib.Header
module F = H.Field
module V = F.Value
module P = V.Parameter

let basic_cont_dis =
  String.join ~sep:"\r\n"
    [ "attachment;";
      "\tfilename*=utf-8''test.gif;";
      "\tfilename=\"test.gif\""
    ]

let of_string_test1 =
  let param1 = P.make "filename*" "utf-8''test.gif" in
  let param2 = P.make ~quotes:true "filename" "test.gif" in
  let expected =
    V.make ~params:[ param1; param2 ] "attachment"
  in
  let real = V.of_string basic_cont_dis in
  let test _ = assert_equal expected real
  in
  "basic of_string of basic_cont_dis test" >:: test

let cd = V.of_string basic_cont_dis

let to_string_test1 =
  check_eq_basic "basic to_string of basic_cont_dis test"
    basic_cont_dis
    (V.to_string cd)

let to_string_test2 =
  check_eq_basic "basic to_string without params"
    "attachment"
    (V.to_string (V.of_string "attachment"))

let lookup_param_test1 =
  let test _ =
    match V.lookup_param "filename" cd with
    | None -> assert_failure "Lookup failed"
    | Some v -> assert_equal "test.gif" v ~printer:id
  in
  "basics lookup test" >:: test

let tests =
  "test suite for header field values"
  >::: [ of_string_test1;
         to_string_test1;
         to_string_test2;
         lookup_param_test1
       ]

let _ = run_test_tt_main tests
