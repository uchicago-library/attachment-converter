open OUnit2
open Lib.Configuration.ParseConfig
open Lib.Configuration.Formats

let check_ok f fname i =
  let description =
    Printf.sprintf
      "%s is Ok on well-formed input"
      fname
  in
  let test _ = i |> f |> Result.is_ok |> assert_bool "actually not Ok" in
  description >:: test

let wf =
  [ ("source_type"  , "application/pdf"   ) ;
    ("target_type"  , "application/pdf"   ) ;
    ("shell_command", "soffice-to-pdfa.sh") ;
  ]

let extra = ("extra", "extra") :: wf

let nwf =
  [ ("source_type"  , "application/pdf"   ) ;
    ("target_type!" , "application/pdf"   ) ;
    ("shell_command", "soffice-to-pdfa.sh") ;
  ]

let wf_to_entry_ok    = check_ok entry_of_assoc "entry_of_assoc" wf
let extra_to_entry_ok = check_ok entry_of_assoc "entry_of_assoc" extra

let check_error f fname i =
  let description =
    Printf.sprintf
      "%s is Error on ill-formed input"
      fname
  in
  let test _ = i |> f |> Result.is_error |> assert_bool "actually not Error" in
  description >:: test

let nwf_to_entry_error = check_error entry_of_assoc "entry_of_assoc" nwf

let check_entry l description st tt ss =
  let l_parsed   = Result.get_ok (entry_of_assoc l)       in
  let st_check _ = assert_equal l_parsed.source_type   st in
  let tt_check _ = assert_equal l_parsed.target_type   tt in
  let ss_check _ = assert_equal l_parsed.shell_command ss in
  description >:::
    [ "source type ok" >:: st_check ;
      "target type ok" >:: tt_check ;
      "script ok"      >:: ss_check ;
    ]

let wf_correct =
  check_entry wf
    "well-formed associativity list converts to entry"
    "application/pdf"
    "application/pdf"
    "soffice-to-pdfa.sh"

let extra_correct =
  check_entry extra
    "well-formed associativity list with extra value converts to entry"
    "application/pdf"
    "application/pdf"
    "soffice-to-pdfa.sh"

let check_trans_data e description tt sc v =
  let td         = transform_data_of_entry e        in
  let tt_check _ = assert_equal td.target_type   tt in
  let ss_check _ = assert_equal td.shell_command sc in
  let  v_check _ = assert_equal td.variety       v  in
  description >:::
    [ "target type ok" >:: tt_check ;
      "script ok"      >:: ss_check ;
      "variety ok"     >::  v_check ;
    ]

let wf_trans_data_correct =
  check_trans_data (Result.get_ok (entry_of_assoc wf))
    "entry with source = target converts to transform_data"
    "application/pdf"
    "soffice-to-pdfa.sh"
    DataAndHeader

let extra_trans_data_correct =
  check_trans_data (Result.get_ok (entry_of_assoc extra))
    "entry with source = target converts to transform_data"
    "application/pdf"
    "soffice-to-pdfa.sh"
    DataAndHeader

let t_neq_s =
  [ ("source_type"  , "a") ;
    ("target_type"  , "b") ;
    ("shell_command", "c") ;
  ]

let t_neq_s_trans_data_correct =
  check_trans_data (Result.get_ok (entry_of_assoc t_neq_s))
    "entry with source /= target converts to transform data"
    "b"
    "c"
    DataAndHeader

let wf_cs =
"%source_type a
%target_type b
%target_extension q
%shell_command c d e

%source_type f
%target_type g
%target_extension q
%shell_command h"

let extra_cs =
"%source_type a
%target_type b
%target_extension q
%shell_command c d e
%test test

%source_type f
%target_type g
%target_extension q
%shell_command h
%test test"

let missing_cs =
"%source_type a
%target_type b

%source_type f
%target_type g
%shell_command h"

let wf_cs_to_data_ok         = check_ok    parse_config_str "parse_config_str" wf_cs
let extra_cs_to_data_ok      = check_ok    parse_config_str "parse_config_str" extra_cs
let missing_cs_to_data_error = check_error parse_config_str "parse_config_str" missing_cs

let e1 =
  { target_type   = "b"           ;
    target_ext    = "q"           ;
    shell_command = "c d e"       ;
    variety       = DataAndHeader ;
  }

let e2 =
  { target_type   = "g"           ;
    target_ext    = "q"           ;
    shell_command = "h"           ;
    variety       = DataAndHeader ;
  }

let check_wf_cs_or_extra_cs cs =
  let description = "checking access for wf_cs/extra_cs"       in
  let open Lib.Configuration.Formats                           in
  let d = Result.get_ok (parse_config_str cs)                  in
  let check key value _ = assert_equal (Dict.find key d) value in
  description >:::
    [ "check wf_cs first entry"  >:: check "a" [e1] ;
      "check wf_cs second entry" >:: check "f" [e2] ;
    ]

let check_error result error =
  let description = "error not as expected"                  in
  let check _ = assert_equal (Result.get_error result) error in
  description >:: check

let missing_cs_error_msg =
  check_error
    (parse_config_str missing_cs)
    (`ConfigData (1, ShellCommand))

let bad_refer_cs =
"%source_type a
%target_type b
%shell_command c d e

not a real line

%source_type f
%target_type g
%shell_command h"

let bad_refer_cs_msg =
  check_error
    (parse_config_str bad_refer_cs)
      (`ReferParse (5, "not a real line"))

let double_entry_cs =
"%source_type a
%target_type b
%target_extension q
%shell_command c d e

%source_type a
%target_type g
%target_extension q
%shell_command h"

let check_double_entry_cs =
  let description = "checking access for double_entry_cs"  in
  let open Lib.Configuration.Formats                       in
  let d = Result.get_ok (parse_config_str double_entry_cs) in
  let check _ = assert_equal (Dict.find "a" d) [e2; e1]    in
  description >:: check



let tests = "test suite for config file parsing" >:::
  [ wf_to_entry_ok                   ;
    extra_to_entry_ok                ;
    nwf_to_entry_error               ;
    wf_correct                       ;
    extra_correct                    ;
    wf_trans_data_correct            ;
    extra_trans_data_correct         ;
    t_neq_s_trans_data_correct       ;
    wf_cs_to_data_ok                 ;
    extra_cs_to_data_ok              ;
    missing_cs_to_data_error         ;
    check_wf_cs_or_extra_cs wf_cs    ;
    check_wf_cs_or_extra_cs extra_cs ;
    missing_cs_error_msg             ;
    bad_refer_cs_msg                 ;
    check_double_entry_cs            ;
  ]

let _ = run_test_tt_main tests
