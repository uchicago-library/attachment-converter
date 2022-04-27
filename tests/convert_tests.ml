open OUnit2
open Lib.Convert.Conversion_ocamlnet

let empty_config_test fname =
  let description = "empty config is noop"
  let email   = readfile fname
  let check _ = assert_equal
    Ok email
    (acopy
      Lib.Configuration.Formats.Dict.empty
      email)

let tests = "test suite for testing conversion" >:::
  [ empty_config_test fname  ;
  ]

let _ = run_test_tt_main tests
