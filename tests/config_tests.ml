open OUnit2
open Lib.Config.ParseConfig

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
    DataOnly

let extra_trans_data_correct =
  check_trans_data (Result.get_ok (entry_of_assoc extra))
    "entry with source = target converts to transform_data"
    "application/pdf"
    "soffice-to-pdfa.sh"
    DataOnly

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
%shell_command c d e

%source_type f
%target_type g
%shell_command h"

let extra_cs =
"%source_type a
%target_type b
%shell_command c d e
%test test

%source_type f
%target_type g
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

let tests = "test suite for config file parsing" >:::
  [ wf_to_entry_ok             ;
    extra_to_entry_ok          ;
    nwf_to_entry_error         ;
    wf_correct                 ;
    extra_correct              ;
    wf_trans_data_correct      ;
    extra_trans_data_correct   ;
    t_neq_s_trans_data_correct ;
    wf_cs_to_data_ok           ;
    extra_cs_to_data_ok        ;
    missing_cs_to_data_error   ;
  ]

let _ = run_test_tt_main tests
