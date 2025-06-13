open OUnit2
open Lib.Mbox_simple

(* Utility: Create an input channel from a string *)
let string_input_channel (s : string) : in_channel =
  let fname = Filename.temp_file "mbox_test" ".txt" in
  let oc = open_out fname in
  output_string oc s;
  close_out oc;
  open_in fname

(* Test 1: Reading a valid single message *)
let test_read_valid_message _ =
  let ic = string_input_channel "From alice@example.com\nHello Alice\nBye\n" in
  let mchan = MBoxParser.create_mchan ic in
  match MBoxParser.read mchan true with
  | Ok msg ->
      let expected = "From alice@example.com\nHello Alice\nBye\n" in
      assert_equal ~printer:(fun x -> x) expected msg
  | Error e -> assert_failure ("Unexpected error: " ^ e);
  close_in ic

(* Test 2: Malformed input without leading "From " line *)
let test_malformed_start _ =
  let ic = string_input_channel "Hello\nFrom bob@example.com\nHey\n" in
  let mchan = MBoxParser.create_mchan ic in
  match MBoxParser.read mchan true with
  | Error msg ->
      assert_bool "Error should describe malformed start" (String.contains msg 'm')
  | Ok _ -> assert_failure "Expected failure due to malformed input";
  close_in ic

(* Test 3: Fold accumulates all valid messages *)
let test_fold_multiple_messages _ =
  let ic = string_input_channel
    "From a@example.com\nOne\nFrom b@example.com\nTwo\nFrom c@example.com\nThree\n" in
  let messages =
    MBoxParser.fold
      (fun acc -> function
        | Ok msg -> msg :: acc
        | Error _ -> acc)
      ic []
    |> List.rev
  in
  let expected = [
    "From a@example.com\nOne\n";
    "From b@example.com\nTwo\n";
    "From c@example.com\nThree\n"
  ] in
  assert_equal ~printer:(String.concat "\n---\n") expected messages;
  close_in ic

(* Test 4: Convert applies transformation to each message *)
let test_convert_applies_function _ =
  let ic =
    string_input_channel
      "From user@example.com\nhello\nFrom another@example.com\nworld\n"
  in

  let tmp_out = Filename.temp_file "mbox_out" ".txt" in
  let orig_stdout = Unix.dup Unix.stdout in
  let fd_out = Unix.openfile tmp_out [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT] 0o600 in
  Unix.dup2 fd_out Unix.stdout;

  let result =
    MBoxParser.convert ic (fun s -> Ok (String.uppercase_ascii s))
  in

  flush stdout;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close fd_out;

  (match result with
  | Ok () -> ()
  | Error err -> assert_failure ("Conversion failed: " ^ err));

  let oc = open_in tmp_out in
  let output = really_input_string oc (in_channel_length oc) in
  close_in oc;

  assert_bool "Output should contain uppercase FROM" (String.contains output 'F');
  assert_bool "Output should contain uppercase HELLO" (String.contains output 'H');

  close_in ic


(* Test 5: Fold handles errors gracefully *)
let test_fold_handles_errors _ =
  let ic = string_input_channel
    "From x@example.com\nok\nOops\nFrom y@example.com\nmsg\n" in
  let results =
    MBoxParser.fold
      (fun acc r -> r :: acc)
      ic []
    |> List.rev
  in
  let oks = List.filter (function Ok _ -> true | _ -> false) results in
  assert_equal ~printer:string_of_int 2 (List.length oks);
  close_in ic

(* Assembling test suite *)
let suite =
  "MBoxParser Test Suite" >::: [
    "read_valid_message" >:: test_read_valid_message;
    "malformed_start" >:: test_malformed_start;
    "fold_multiple_messages" >:: test_fold_multiple_messages;
    "convert_applies_function" >:: test_convert_applies_function;
    "fold_handles_errors" >:: test_fold_handles_errors;
  ]

let () = run_test_tt_main suite
