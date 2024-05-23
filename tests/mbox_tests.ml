open OUnit2
open Lib.Mbox
open Prelude

let test_lines = "line 1\r\nline 2\r\nline 3\r\n"

let line_iterator_test_1 =
  let description =
    "Check multiple lines of line iterator"
  in
  let module L = StringInput in
  let l = L.create test_lines in
  let line_1 = L.next l in
  let line_2 = L.next l in
  let line_3 = L.next l in
  let check_1 _ =
    assert_equal "line 1" line_1 ~printer:id
  in
  let check_2 _ =
    assert_equal "line 2" line_2 ~printer:id
  in
  let check_3 _ =
    assert_equal "line 3" line_3 ~printer:id
  in
  description
  >::: [ "line 1 check" >:: check_1;
         "line 2 check" >:: check_2;
         "line 3 check" >:: check_3
       ]

(* Written by Owen *)
let to_mbox ?(escape = false) ?(eol = "\n")
    ?(fromline = "From BLAH") =
  let fromline = fromline ^ eol in
  if escape
  then
    let open Strings in
    let escape_froms =
      replace (eol ^ "From ") (eol ^ ">From ")
      << replace (eol ^ ">From ") (eol ^ ">>From ")
    in
    fold_left
      (fun acc email -> acc ^ fromline ^ email ^ eol)
      ""
    << map escape_froms
  else
    fold_left
      (fun acc email -> acc ^ fromline ^ email ^ eol)
      ""

let dummy_mbox =
  to_mbox ~eol:"\r\n" [ "Email 1"; "Email 2"; "Email 3" ]

let dummy_mbox_expected =
  "From BLAH\r\n\
   Email 1\r\n\
   From BLAH\r\n\
   Email 2\r\n\
   From BLAH\r\n\
   Email 3\r\n"

let to_mbox_test_1 =
  let description = "basic to_mbox test" in
  let check _ =
    assert_equal dummy_mbox_expected dummy_mbox
  in
  description >:: check

let to_mbox_test_2 =
  let description = "test_email to_mbox test" in
  let email = "THIS IS A DUMMY EMAIL" in
  let mbox = to_mbox ~eol:"\r\n" [ email; email ] in
  let check _ =
    assert_equal
      ( "From BLAH\r\n" ^ email ^ "\r\nFrom BLAH\r\n"
      ^ email ^ "\r\n" )
      mbox ~printer:id
  in
  description >:: check

let mbox_iter_test_1 =
  let description = "basic mbox iterator test" in
  let module T = MBoxIterator (StringInput) in
  let t = T.create dummy_mbox_expected in
  let f1, m1 = T.next t in
  let f2, m2 = T.next t in
  let f3, m3 = T.next t in
  let checkf1 _ = assert_equal "From BLAH" f1 ~printer:id in
  let checkf2 _ = assert_equal "From BLAH" f2 ~printer:id in
  let checkf3 _ = assert_equal "From BLAH" f3 ~printer:id in
  let checkm1 _ =
    assert_equal "Email 1\r\n" m1 ~printer:id
  in
  let checkm2 _ =
    assert_equal "Email 2\r\n" m2 ~printer:id
  in
  let checkm3 _ =
    assert_equal "Email 3\r\n" m3 ~printer:id
  in
  description
  >::: [ "fromline check" >:: checkf1;
         "fromline check" >:: checkf2;
         "fromline check" >:: checkf3;
         "email check" >:: checkm1;
         "email check" >:: checkm2;
         "email check" >:: checkm3
       ]

let basic_convert_test f expected description =
  let module T =
    Conversion (MBoxIterator (StringInput)) (StringOutput)
  in
  let output = T.convert dummy_mbox_expected () f in
  let check _ = assert_equal expected output ~printer:id in
  description >:: check

let convert_test_1 =
  basic_convert_test (k "") ""
    "convert mbox to empty string"

let convert_test_2 =
  basic_convert_test
    (fun (x, y) -> x ^ "\r\n" ^ y)
    dummy_mbox_expected
    "identity conversion (with trailing newline) on mbox"

let convert_test_3 =
  let description =
    "read in file and apply identity conversion"
  in
  let module T =
    Conversion (MBoxIterator (StringInput)) (StringOutput)
  in
  let email = "testing\r\ntesting" in
  let mbox = to_mbox ~eol:"\r\n" [ email; email ] in
  let output =
    T.convert mbox () (fun (x, y) -> x ^ "\r\n" ^ y)
  in
  let check _ = assert_equal mbox output ~printer:id in
  description >:: check

let tests =
  "test suite for mbox conversion"
  >::: [ line_iterator_test_1;
         to_mbox_test_1;
         to_mbox_test_2;
         mbox_iter_test_1;
         convert_test_1;
         convert_test_2;
         convert_test_3
       ]

let _ = run_test_tt_main tests
