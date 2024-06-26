open OUnit2
open Utils
open Lib
open Lib.Configuration

let wf =
  [ ("source_type", "application/pdf");
    ("target_type", "application/pdf");
    ("shell_command", "soffice-to-pdfa.sh");
    ("id", "id")
  ]

let extra = ("extra", "extra") :: wf

let nwf =
  [ ("source_type", "application/pdf");
    ("target_type!", "application/pdf");
    ("shell_command", "soffice-to-pdfa.sh")
  ]

let wf_to_entry_ok =
  check_is_ok
    (ConfigEntry.of_refer wf)
    "(entry_of_assoc wf)"

let extra_to_entry_ok =
  check_is_ok
    (ConfigEntry.of_refer extra)
    "(entry_of_assoc extra)"

let nwf_to_entry_error =
  check_is_error (ConfigEntry.of_refer nwf) "entry_of_assoc"

let check_entry l description st tt ss =
  let open ConfigEntry in
  let l_parsed = Result.get_ok (of_refer l) in
  let st_check _ = assert_equal (source_type l_parsed) st in
  let tt_check _ = assert_equal (target_type l_parsed) tt in
  let ss_check _ =
    assert_equal (shell_command l_parsed) ss
  in
  description
  >::: [ "source type ok" >:: st_check;
         "target type ok" >:: tt_check;
         "script ok" >:: ss_check
       ]

let wf_correct =
  check_entry wf
    "well-formed associativity list converts to entry"
    Mime_type.pdf Mime_type.pdf "soffice-to-pdfa.sh"

let extra_correct =
  check_entry extra
    "well-formed associativity list with extra value \
     converts to entry"
    Mime_type.pdf Mime_type.pdf "soffice-to-pdfa.sh"

let check_trans_data e description tt sc =
  let open TransformData in
  let td =
    Result.get_ok (TransformData.of_config_entry e)
  in
  let tt_check _ = assert_equal (target_type td) tt in
  let ss_check _ = assert_equal (shell_command td) sc in
  description
  >::: [ "target type ok" >:: tt_check;
         "script ok" >:: ss_check
       ]

let wf_trans_data_correct =
  check_trans_data
    (Result.get_ok (ConfigEntry.of_refer wf))
    "entry with source = target converts to transform_data"
    Mime_type.pdf "soffice-to-pdfa.sh"

let extra_trans_data_correct =
  check_trans_data
    (Result.get_ok (ConfigEntry.of_refer extra))
    "entry with source = target converts to transform_data"
    Mime_type.pdf "soffice-to-pdfa.sh"

let t_neq_s =
  [ ("source_type", "application/pdf");
    ("target_type", "text/plain");
    ("shell_command", "c");
    ("id", "id")
  ]

let t_neq_s_trans_data_correct =
  check_trans_data
    (Result.get_ok (ConfigEntry.of_refer t_neq_s))
    "entry with source /= target converts to transform data"
    Mime_type.txt "c"

let wf_cs =
  "%source_type application/pdf\n\
   %target_type text/plain\n\
   %target_ext q\n\
   %shell_command c d e\n\
   %id id\n\n\
   %source_type image/tiff\n\
   %target_type image/bmp\n\
   %target_ext q\n\
   %shell_command h\n\
   %id id"

let extra_cs =
  "%source_type application/pdf\n\
   %target_type text/plain\n\
   %target_ext q\n\
   %shell_command c d e\n\
   %test test\n\
   %id id\n\n\
   %source_type image/tiff\n\
   %target_type image/bmp\n\
   %target_ext q\n\
   %shell_command h\n\
   %test test\n\
   %id id"

let missing_cs =
  "%source_type application/pdf\n\
   %target_type application/pdf\n\n\
   %source_type application/pdf\n\
   %target_type application/pdf\n\
   %shell_command h"

let wf_cs_to_data_ok =
  check_is_ok (Formats.of_string wf_cs) "(parse wf_cs)"

let extra_cs_to_data_ok =
  check_is_ok
    (Formats.of_string extra_cs)
    "(parse extra_cs)"

let missing_cs_to_data_error =
  check_is_error
    (Formats.of_string missing_cs)
    "(parse missing_cs)"

let e1 =
  TransformData.make ~target_type:Mime_type.txt
    ~target_ext:"q" ~shell_command:"c d e" ~convert_id:"id"

let e2 =
  TransformData.make ~target_type:Mime_type.bmp
    ~target_ext:"q" ~shell_command:"h" ~convert_id:"id"

let check_wf_cs_or_extra_cs cs =
  let description = "checking access for wf_cs/extra_cs" in
  let open Formats in
  let d = Result.get_ok (of_string cs) in
  let check key value _ =
    assert_equal (conversions d key) value
  in
  description
  >::: [ "check wf_cs first entry"
         >:: check Mime_type.pdf [ e1 ];
         "check wf_cs second entry"
         >:: check Mime_type.tiff [ e2 ]
       ]

let missing_cs_error_msg =
  let error =
    Result.get_error (Formats.of_string missing_cs)
  in
  check_eq_basic "Error not as expected, wanted ConfigData"
    error
    (`ConfigData (1, `ShellCommand))

let bad_refer_cs =
  "%source_type a\n\
   %target_type b\n\
   %shell_command c d e\n\
   %id id\n\n\
   not a real line\n\n\
   %source_type f\n\
   %target_type g\n\
   %shell_command h\n\
   %id id"

let bad_refer_cs_msg =
  check_eq_basic "Error not as expected, wanted ReferParse"
    (Result.get_error (Formats.of_string bad_refer_cs))
    (`ReferParse (6, "not a real line"))

let double_entry_cs =
  "%source_type application/pdf\n\
   %target_type text/plain\n\
   %target_ext q\n\
   %shell_command c d e\n\
   %id id\n\n\
   %source_type application/pdf\n\
   %target_type image/bmp\n\
   %target_ext q\n\
   %shell_command h\n\
   %id id"

let check_double_entry_cs =
  let open Formats in
  let d = Result.get_ok (of_string double_entry_cs) in
  check_eq_basic "check access for double_entry_cs"
    (conversions d Mime_type.pdf)
    [ e2; e1 ]

let tests =
  "test suite for config file parsing"
  >::: [ wf_to_entry_ok;
         extra_to_entry_ok;
         nwf_to_entry_error;
         wf_correct;
         extra_correct;
         wf_trans_data_correct;
         extra_trans_data_correct;
         t_neq_s_trans_data_correct;
         wf_cs_to_data_ok;
         extra_cs_to_data_ok;
         missing_cs_to_data_error;
         check_wf_cs_or_extra_cs wf_cs;
         check_wf_cs_or_extra_cs extra_cs;
         missing_cs_error_msg;
         bad_refer_cs_msg;
         check_double_entry_cs
       ]

let _ = run_test_tt_main tests
