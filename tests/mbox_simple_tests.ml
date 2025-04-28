open OUnit2
open Lib.Mbox_simple

let string_input_channel str =
  let tmp_file = Filename.temp_file "mbox_test" ".txt" in
  let oc = open_out tmp_file in
  output_string oc str;
  close_out oc;
  open_in tmp_file

let test_peek_line _ =
  let ic = string_input_channel "From alice@example.com\nBody line 1\n" in
  let mchan = MBoxChannel.create ic in
  let line1, _ = MBoxChannel.peek_line mchan in
  let line2, _ = MBoxChannel.peek_line mchan in
  assert_equal line1 line2;
  close_in ic

let test_read_line_and_buffer _ =
  let ic = string_input_channel "From bob@example.com\nLine 1\nLine 2\n" in
  let mchan = MBoxChannel.create ic in
  ignore (MBoxChannel.read_line mchan); 
  ignore (MBoxChannel.read_line mchan); 
  let _, _ = MBoxChannel.peek_line mchan in
  ignore (MBoxChannel.read_line mchan);
  match MBoxChannel.get_contents mchan with
  | Ok contents ->
      assert_equal "From bob@example.com\nLine 1\nLine 2\n" contents
  | Error _ -> ();
  close_in ic

let test_malformed_from_line _ =
  let ic = string_input_channel "Invalid start\nFrom bob@example.com\nLine\n" in
  let mchan = MBoxChannel.create ic in
  match read_email mchan true with
  | Error msg -> assert_bool "should contain malformed error" (String.contains msg 'm')
  | Ok _ -> assert_failure "Expected error due to malformed from line"

let test_valid_single_email _ =
  let ic = string_input_channel "From charlie@example.com\nHello\nWorld\nFrom delta@example.com\n" in
  let mchan = MBoxChannel.create ic in
  match read_email mchan true with
  | Ok email -> assert_equal "From charlie@example.com\nHello\nWorld\n" email
  | Error _ -> ();
  close_in ic

(* let test_read_all_mbox_collects_all _ =
  let emails = ref [] in
  let ic = string_input_channel "From a@example.com\nBody 1\nFrom b@example.com\nBody 2\nFrom c@example.com\nBody 3\n" in
  read_all_mbox ic (fun e -> ());
  assert_equal 3 (List.length !emails);
  close_in ic

let test_mbox_file_fold _ =
  let ic = string_input_channel
    "From one@example.com\nFirst message\nFrom two@example.com\nSecond message\n" in
  let acc = mbox_file_fold (fun acc -> function
    | Ok msg -> msg :: acc
    | Error _ -> acc) ic [] in
  assert_equal 2 (List.length acc);
  assert_bool "should include 'First message'" (List.exists (fun m -> String.contains m 'F') acc);
  close_in ic *)

let suite =
  "MBoxChannel Tests" >::: [
    "peek_line" >:: test_peek_line;
    "read_line_and_buffer" >:: test_read_line_and_buffer;
    "malformed_from_line" >:: test_malformed_from_line;
    "valid_single_email" >:: test_valid_single_email;
    (* "read_all_mbox" >:: test_read_all_mbox_collects_all;
    "mbox_file_fold" >:: test_mbox_file_fold; *)
  ]

let () = run_test_tt_main suite
