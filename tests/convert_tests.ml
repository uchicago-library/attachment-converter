open OUnit2
open Lib.Convert.Conversion_ocamlnet
open Prelude

let empty_config_test_0 fname =
  let description = "empty config is okay on email" in
  let email   = readfile fname in
  let check _ = assert_bool "is okay"
    (Result.good (acopy_email
      Lib.Configuration.Formats.Dict.empty
      email))
  in
  description >:: check

let empty_config_test_1 fname =
  let description = "empty config is noop" in
  let email   = readfile fname in
  let check _ = assert_equal
    (Ok (parse email))
    (acopy
      Lib.Configuration.Formats.Dict.empty
      (parse email))
  in
  description >:: check


let tests = "test suite for testing conversion" >:::
  [ empty_config_test_0 "test_emails/xmas_email" ;
    empty_config_test_1 "test_emails/xmas_email" ;
  ]

let _ = run_test_tt_main tests
