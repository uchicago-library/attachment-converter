open OUnit2
open Lib.Mbox
open Prelude

(* Written by Owen *)
let to_mbox ?(escape=false) ?(eol="\n") =
  let fromline = "From jorge@babel.lib Thu Aug 24 12:00:00 1899" ^ eol in
  if escape then
    let open Strings in
    let escape_froms = replace (eol ^ "From ") (eol ^ ">From ") << replace (eol ^ ">From ") (eol ^ ">>From ") in
    fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) "" << map escape_froms
  else
    fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) ""

let test_email = readfile "test_email"

let single_email_test_mbox = to_mbox [test_email]

let 

let tests = "test suite for mbox conversion" >:::
  [ dumb_test ;
  ]

let _ = run_test_tt_main tests
