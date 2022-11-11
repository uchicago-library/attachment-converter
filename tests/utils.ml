open OUnit2
open Prelude

let check_is_something x name =
  let description =
    Printf.sprintf
      "%s is something"
      name
  in
  let error_str =
    Printf.sprintf
      "actual %s is none"
      name
  in
    let test _ = x |> Option.something |> assert_bool error_str in
    description >:: test

let check_is_ok x name =
  let description =
    Printf.sprintf
      "%s is Ok"
      name
  in
  let error_str =
    Printf.sprintf
      "actually %s is not Ok"
      name
  in
    let test _ = x |> Result.good |> assert_bool error_str in
    description >:: test

let check_is_error x name =
  let description =
    Printf.sprintf
      "%s is Error"
      name
  in
  let error_str =
    Printf.sprintf
      "actually %s is not Error"
      name
  in
  let test _ = x |> Result.bad |> assert_bool error_str in
  description >:: test

let check_eq_basic description expected actual =
  let test _ = assert_equal expected actual in
    description >:: test

let check_eq_string description expected actual =
  let test _ = assert_equal expected actual ~printer:id in
    description >:: test
