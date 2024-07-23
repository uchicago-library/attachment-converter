open OUnit2
open Utils
open Lib
open Lib.Configuration
open Prelude

let wf_refer_entry =
  [ ("source_type", "application/pdf");
    ("target_type", "application/pdf");
    ("shell_command", "soffice-to-pdfa.sh");
    ("id", "id")
  ]

let extra = ("extra", "extra") :: wf_refer_entry

let nwf_refer_entry =
  [ ("source_type", "application/pdf");
    ("target_type!", "application/pdf");
    ("shell_command", "soffice-to-pdfa.sh")
  ]

let wf_to_entry_ok =
  check_is_ok
    (Config_entry.of_refer wf_refer_entry)
    "(of_refer wf_refer_entry)"

let extra_to_entry_ok =
  check_is_ok
    (Config_entry.of_refer extra)
    "(of_refer extra)"

let nwf_to_entry_error =
  check_is_error
    (Config_entry.of_refer nwf_refer_entry)
    "(of_refer nwf_refer_entry)"

let check_entry l description st tt ss =
  let open Config_entry in
  let open Result in
  let l_parsed = of_refer l in
  let st_check _ = assert_equal (map source_type l_parsed) (Ok st) in
  let tt_check _ = assert_equal (map target_type l_parsed) (Ok tt) in
  let ss_check _ = assert_equal (map shell_command l_parsed) (Ok ss) in
  description
  >::: [ "source type ok" >:: st_check;
         "target type ok" >:: tt_check;
         "script ok" >:: ss_check
       ]

let wf_correct =
  check_entry wf_refer_entry
    "well-formed associativity list converts to entry"
    Mime_type.pdf Mime_type.pdf "soffice-to-pdfa.sh"

let extra_correct =
  check_entry extra
    "well-formed associativity list with extra value \
     converts to entry"
    Mime_type.pdf Mime_type.pdf "soffice-to-pdfa.sh"

let check_trans_data e description tt sc =
  let open TransformData in
  let open Result in
  let td = bind e TransformData.of_config_entry in
  let tt_check _ = assert_equal (map target_type td) (Ok tt) in
  let ss_check _ = assert_equal (map shell_command td) (Ok sc) in
  description
  >::: [ "target type ok" >:: tt_check;
         "script ok" >:: ss_check
       ]

let wf_trans_data_correct =
  check_trans_data
    (Config_entry.of_refer wf_refer_entry)
    "entry with source = target converts to transform_data"
    Mime_type.pdf "soffice-to-pdfa.sh"

let extra_trans_data_correct =
  check_trans_data
    (Config_entry.of_refer extra)
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
    (Config_entry.of_refer t_neq_s)
    "entry with source /= target converts to transform data"
    Mime_type.txt "c"

let wf_cs =
  String.join ~sep:"\n"
    [ "%source_type application/pdf"
    ; "%target_type text/plain"
    ; "%target_ext q"
    ; "%shell_command c d e"
    ; "%id id"
    ; ""
    ; "%source_type image/tiff"
    ; "%target_type image/bmp"
    ; "%target_ext q"
    ; "%shell_command h"
    ; "%id id"
    ]

let extra_cs =
  String.join ~sep:"\n"
  [ "%source_type application/pdf"
  ; "%target_type text/plain"
  ; "%target_ext q"
  ; "%shell_command c d e"
  ; "%test test"
  ; "%id id"
  ; ""
  ; "%source_type image/tiff"
  ; "%target_type image/bmp"
  ; "%target_ext q"
  ; "%shell_command h"
  ; "%test test"
  ; "%id id"
  ]

let missing_cs =
  String.join ~sep:"\n"
    [ "%source_type application/pdf"
    ; "%target_type application/pdf"
    ; ""
    ; "%source_type application/pdf"
    ; "%target_type application/pdf"
    ; "%shell_command h"
    ]

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
  let open Result in
  let d = of_string cs in
  let check key value _ =
    let printer option_td =
    match option_td with
    | Error e -> "\n" ^ Error_message.debug e ^ "\n"
    | Ok tds ->
      "\n" ^ Prelude.String.join ~sep:"\n\n"
        (List.map
           Configuration.TransformData.to_string
           tds) ^ "\n"
    in
    assert_equal (Ok value) (map (fun x -> conversions x key) d) ~printer
  in
  description
  >::: [ "check wf_cs first entry"
         >:: check Mime_type.pdf [ e1 ];
         "check wf_cs second entry"
         >:: check Mime_type.tiff [ e2 ]
       ]

let missing_cs_error_msg =
  let error = Formats.of_string missing_cs
  in
  check_eq_basic "Error not as expected, wanted ConfigData"
    (Prelude.Result.witherr List.hd error) (Error (`ConfigData 1))

let bad_refer_cs =
  String.join ~sep:"\n"
  [ "%source_type a"
  ; "%target_type b"
  ; "%shell_command c d e"
  ; "%id id"
  ; ""
  ; "not a real line"
  ; ""
  ; "%source_type f"
  ; "%target_type g"
  ; "%shell_command h"
  ; "%id id"
  ]

let bad_refer_cs_msg =
  check_eq_basic
    "Error not as expected, wanted\n   ReferParse"
    (Formats.of_string bad_refer_cs)
    (Error [ `ReferParse (6, "not a real line") ])

let double_entry_cs =
  String.join ~sep:"\n"
  [ "%source_type application/pdf"
  ; "%target_type text/plain"
  ; "%target_ext q"
  ; "%shell_command c d e"
  ; "%id id"
  ; ""
  ; "%source_type application/pdf"
  ; "%target_type image/bmp"
  ; "%target_ext q"
  ; "%shell_command h"
  ; "%id id"
  ]

let check_double_entry_cs =
  let open Formats in
  let printer option_td =
    match option_td with
    | Error e -> "\n" ^ Error_message.debug e ^ "\n"
    | Ok tds ->
      "\n" ^ Prelude.String.join ~sep:"\n\n"
        (List.map
           Configuration.TransformData.to_string
           tds) ^ "\n"
  in
  let expected = Ok [ e2; e1 ] in
  let real =
    let ( let* ) = Result.bind in
    let* d = of_string double_entry_cs in
    Ok (conversions d Mime_type.pdf)
  in
  let test _ = assert_equal expected real ~printer in
  "check access for double_entry_cs" >:: test

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
