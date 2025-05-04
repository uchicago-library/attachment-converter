open OUnit2
open Prelude
open Lib.Mbox_simple

let string_input_channel str =
  let tmp_file = Filename.temp_file "mbox_test" ".txt" in
  let oc = open_out tmp_file in
  output_string oc str;
  close_out oc;
  open_in tmp_file

  module FileChannel : Channel with type t = in_channel =  struct
    type t = in_channel
    let input_line = input_line
  end
  
  module FileMBoxChannel = MBoxChannel(FileChannel)
  module FileMBoxReader = MboxReader(FileMBoxChannel)

  let test_peek_line _ =
    let ic = string_input_channel "From alice@example.com\nBody line 1\n" in
    let mchan = FileMBoxChannel.create ic in
    let line1, _ = FileMBoxChannel.peek_line mchan in
    let line2, _ = FileMBoxChannel.peek_line mchan in
    assert_equal line1 line2;
    close_in ic
  

let test_read_line_and_buffer _ =
  let ic = string_input_channel "From bob@example.com\nLine 1\nLine 2\n" in
  let mchan = FileMBoxChannel.create ic in
  ignore (FileMBoxChannel.read_line mchan); 
  ignore (FileMBoxChannel.read_line mchan); 
  let _, _ = FileMBoxChannel.peek_line mchan in
  ignore (FileMBoxChannel.read_line mchan);
  match FileMBoxChannel.get_contents mchan with
  | Ok contents ->
      assert_equal "From bob@example.com\nLine 1\nLine 2\n" contents
  | Error _ -> ();
  close_in ic

let test_malformed_from_line _ =
  let ic = string_input_channel "Invalid start\nFrom bob@example.com\nLine\n" in
  let mchan = FileMBoxChannel.create ic in
  match FileMBoxReader.read_email mchan true with
  | Error msg -> assert_bool "should contain malformed error" (String.contains msg 'm')
  | Ok _ -> assert_failure "Expected error due to malformed from line"

let test_valid_single_email _ =
  let ic = string_input_channel "From charlie@example.com\nHello\nWorld\nFrom delta@example.com\n" in
  let mchan = FileMBoxChannel.create ic in
  match FileMBoxReader.read_email mchan true with
  | Ok email -> assert_equal "From charlie@example.com\nHello\nWorld\n" email
  | Error _ -> ();
  close_in ic

let test_mbox_file_fold _ =
  let ic = string_input_channel
    "From one@example.com\nFirst message\nFrom two@example.com\nSecond message\n" in
  let acc = FileMBoxReader.mbox_file_fold (fun acc -> function
    | Ok msg -> msg :: acc
    | Error _ -> acc) ic [] in
  assert_equal 2 (List.length acc);
  assert_bool "should include 'First message'" (List.exists (fun m -> String.contains m 'F') acc);
  close_in ic

let suite =
  "MBoxChannel Tests" >::: [
    "peek_line" >:: test_peek_line;
    "read_line_and_buffer" >:: test_read_line_and_buffer;
    "malformed_from_line" >:: test_malformed_from_line;
    "valid_single_email" >:: test_valid_single_email;
    "mbox_file_fold" >:: test_mbox_file_fold;
  ]

let () = run_test_tt_main suite
