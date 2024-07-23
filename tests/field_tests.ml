open OUnit2
open Utils
open Lib.Header.Field

let str1 = "Test: test;\r\n\ta1=v1;\r\n\ta2=\"v2\""

let fld1 =
  { name = "Test";
    value =
      { value = "test";
        params =
          [ { attr = "a1"; value = "v1"; quotes = false };
            { attr = "a2"; value = "v2"; quotes = true }
          ]
      }
  }

let str2 = " Test : test ; \r\n \ta1=v1; \r\n\ta2=\"v2\""

let of_string_test1 =
  check_eq_basic "basic of_string test" (Ok fld1)
    (of_string str1)

let of_string_test2 =
  check_is_ok (of_string str2) "(of_string str2)"

let of_string_test3 =
  check_eq_basic "of_string with spaces test" (Ok fld1)
    (of_string str2)

let to_string_test1 =
  check_eq_string "basic to_string test" str1
    (to_string fld1)

let to_assoc_test1 =
  check_eq_basic "basic to_assoc test"
    ("Test", "test;\r\n\ta1=v1;\r\n\ta2=\"v2\"")
    (to_assoc fld1)

let tests =
  "test suite for fields"
  >::: [ of_string_test1;
         of_string_test2;
         of_string_test3;
         to_string_test1;
         to_assoc_test1
       ]

let _ = run_test_tt_main tests
