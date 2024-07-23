open OUnit2
open Utils
module H = Lib.Header
module F = H.Field
module V = F.Value
module P = V.Parameter

let str1 =
  "Test1: test1;\r\n\
   \ta1=v1\r\n\
   Test2: test2;\r\n\
   \ta2=\"v2\"\r\n"

let v1 = V.make ~params:[ P.make "a1" "v1" ] "test1"

let f1 = F.make "Test1" v1

let f2 = F.make "Test2"
    (V.make ~params:[ P.make ~quotes:true "a2" "v2" ] "test2")

let f3 = F.make "Test2" v1
let hd1 = H.of_list [ f1; f2 ]
let hd2 = H.of_list [ f1 ]
let hd3 = H.of_list [ f1; f3 ]

let asc1 =
  [ ("Test1", "test1;\r\n\ta1=v1");
    ("Test2", "test2;\r\n\ta2=\"v2\"")
  ]

let of_assoc_list_test1 =
  check_is_ok (H.of_assoc_list asc1) "(of_assoc_list asc1)"

let of_assoc_list_test2 =
  check_eq_basic "basic of_assoc_list test" (Ok hd1)
    (H.of_assoc_list asc1)

let to_assoc_list_test1 =
  check_eq_basic "basic to_assoc_list test" asc1
    (H.to_assoc_list hd1)

let to_string_test1 =
  check_eq_string "basic to_string test" str1
    (H.to_string hd1)

let update_test1 =
  check_eq_basic "basic remove test" hd2
    (H.update (fun _ -> None) "Test2" hd1)

let update_test2 =
  check_eq_basic "basic update test" hd3
    (H.update (fun _ -> Some v1) "Test2" hd1)

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
