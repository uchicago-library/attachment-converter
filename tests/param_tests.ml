open OUnit2
open Prelude
open Utils
open Lib.Header.Field.Value.Parameter

let pmake = Lib.Header.Field.Value.Parameter.make

let p1 = pmake
  ~quotes:false
  "param1"
  "value1"

let p2 = pmake
  ~quotes:true
  "param2"
  "value2"

let attr_test1 = check_eq_basic "basic attr test" "param1" (attr p1)
let value_test1 = check_eq_basic "basic value test" "value2" (value p2)

let value_test2 =
  check_eq_basic
    "value access with quotes"
    "\"value2\""
    (value ~quotes:true p2)

let map_test1 =
  check_eq_basic
  "basic map_attr test"
    "testing"
    (attr (map_attr (k "testing") p1))

let map_test2 = 
  check_eq_basic
    "basic map_value test"
    "testing"
    (value (map_value (k "testing") p2))

let p_str1 = "param1=value1"
let p_str2 = "p=\"v\""

let of_string_test1 = check_is_something (of_string p_str1) "(of_string p_str1)"

let of_string_test2 =
  let out = Option.get (of_string p_str1) in
  "of string, no quotes" >:::
    [ check_eq_basic "attr check" "param1" (attr out);
      check_eq_basic "value check" "value1" (value out);
    ]

let of_string_test3 = check_is_something (of_string p_str2) "(of_string p_str2)"

let of_string_test4 =
  let out = Option.get (of_string p_str2) in
  "of string, with quotes" >:::
    [ check_eq_basic "attr check"  "p" (attr out);
      check_eq_basic "value check" "v" (value out);
    ]

let to_string_test1 =
  check_eq_basic
    "basic to_string test, no quotes"
    "param1=value1"
    (to_string p1)

let to_string_test2 =
  check_eq_basic
    "basic to_string test, with quotes"
    "param2=\"value2\""
    (to_string p2)

let to_string_test3 =
  check_eq_basic
    "stronger to_string test"
    "alsd-23%laksdlk=n,,sdfkj32ff234"
    (to_string
      (pmake
        "alsd-23%laksdlk"
        "n,,sdfkj32ff234"))

let tests = "test suite for header" >:::
  [ attr_test1 ;
    value_test1 ;
    value_test2 ;
    map_test1 ;
    map_test2 ;
    of_string_test1 ;
    of_string_test2 ;
    of_string_test3 ;
    of_string_test4 ;
    to_string_test1 ;
    to_string_test2 ;
    to_string_test3 ;
  ]

let _ = run_test_tt_main tests
