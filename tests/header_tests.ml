open OUnit2
open Utils
open Lib.Header.Field.Value
open Lib.Header.Field
open Lib.Header

let str1 =
  "Test1: test1;\r\n\
  \ \ta1=v1\r\n\
  \ Test2:\n\
  \   test2;\r\n\
  \ \ta2=\"v2\"\r\n"

let v1 =
  { value = "test1";
    params =
      [ { attr = "a1"; value = "v1"; quotes = false } ]
  }

let f1 = { name = "Test1"; value = v1 }

let f2 =
  { name = "Test2";
    value =
      { value = "test2";
        params =
          [ { attr = "a2"; value = "v2"; quotes = true } ]
      }
  }

let f3 = { name = "Test2"; value = v1 }
let hd1 = of_list [ f1; f2 ]
let hd2 = of_list [ f1 ]
let hd3 = of_list [ f1; f3 ]

let asc1 =
  [ ("Test1", "test1;\r\n\ta1=v1");
    ("Test2", "test2;\r\n\ta2=\"v2\"")
  ]

let of_assoc_list_test1 =
  check_is_ok
    (of_assoc_list asc1)
    "(of_assoc_list\n   asc1)"

let of_assoc_list_test2 =
  check_eq_basic "basic of_assoc_list test" (Ok hd1)
    (of_assoc_list asc1)

let to_assoc_list_test1 =
  check_eq_basic "basic to_assoc_list test" asc1
    (to_assoc_list hd1)

let to_string_test1 =
  check_eq_string "basic to_string test" str1
    (to_string hd1)

let update_test1 =
  check_eq_basic "basic remove test" hd2
    (update (fun _ -> None) "Test2" hd1)

let update_test2 =
  check_eq_basic "basic update test" hd3
    (update (fun _ -> Some v1) "Test2" hd1)

let tests =
  "test suite for fields"
  >::: [ of_assoc_list_test1;
         of_assoc_list_test2;
         to_assoc_list_test1;
         to_string_test1;
         update_test1;
         update_test2
       ]

let _ = run_test_tt_main tests
