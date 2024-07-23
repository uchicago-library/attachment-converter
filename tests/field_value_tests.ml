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
  let real_result = V.of_string basic_cont_dis in
  let test _ =
    match real_result with
    | Error _ -> assert_failure "got an error"
    | Ok real -> assert_equal expected real
  in
  "basic of_string of basic_cont_dis test" >:: test

let cd = V.of_string basic_cont_dis

let to_string_test1 =
  check_eq_basic "basic to_string of basic_cont_dis test"
    (Ok basic_cont_dis)
    (Result.map V.to_string cd)

let to_string_test2 =
  check_eq_basic "basic to_string without params"
    (Ok "attachment")
    (Result.map V.to_string (V.of_string "attachment"))

let lookup_param_test1 =
  let test _ =
    match cd with
    | Error _ -> assert_failure "cd is none"
    | Ok cd -> (
      match V.lookup_param "filename" cd with
      | None -> assert_failure "Lookup failed"
      | Some v -> assert_equal "test.gif" v ~printer:id )
  in
  "basics lookup test" >:: test

let update_tests =
  let param1 = P.make "filename*" "utf-8''test.gif" in
  let param2 = P.make ~quotes:true "filename" "test.gif" in
  let param3 = P.make "filename" "ok.gif" in
  let param4 = P.make ~quotes:true "whatever" "ok.gif" in
  let test1 =
    check_eq_basic "basic update_value test"
      (Ok (V.make ~params:[ param1; param2 ] "inline"))
      (Result.map (V.update_value "inline") cd)
  in
  let test2 =
    check_eq_basic "basic update_param test"
      (Result.map
         (V.make ~params:[ param1; param3 ] << V.value)
         cd )
      (Result.map
         (V.update_param "filename" (fun _ -> Some param3))
         cd )
  in
  let test3 =
    check_eq_basic "basic update_param remove test"
      (Result.map (V.make ~params:[ param1 ] << V.value) cd)
      (Result.map
         (V.update_param "filename" (fun _ -> None))
         cd )
  in
  let test4 =
    check_eq_basic "basic update_param no op test" cd
      (Result.map
         (V.update_param "file" (fun _ -> None))
         cd )
  in
  let test5 =
    check_eq_basic "basic add_param test"
      (Result.map
         ( V.make ~params:[ param1; param2; param4 ]
         << V.value )
         cd )
      (Result.map
         (V.add_param ~quotes:true "whatever" "ok.gif")
         cd )
  in
  let test6 =
    check_eq_basic "basic add_param with quotes test"
      (Result.map
         ( V.make
             ~params:
               [ param1;
                 param2;
                 P.make ~quotes:true (P.attr param4)
                   (P.value param4)
               ]
         << V.value )
         cd )
      (Result.map
         (V.add_param ~quotes:true "whatever" "ok.gif")
         cd )
  in
  let test7 =
    check_eq_basic "add_param as update test"
      (Result.map
         (V.make ~params:[ param1; param3 ] << V.value)
         cd )
      (Result.map (V.add_param "filename" "ok.gif") cd)
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
