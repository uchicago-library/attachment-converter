open OUnit2
open Lib.Config.ParseConfig

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

let t_neq_s =
  [ ("source_type"  , "a") ;
    ("target_type"  , "b") ;
    ("shell_command", "c") ;
  ]

let t_neq_s_extra = ("d", "d") :: t_neq_s

let parses l =
  let description = "well-formed associativity list parse is Ok"                in
  let test _      = l |> entry_of_assoc |> Result.is_ok |> assert_bool "not Ok" in
  description >:: test

let no_parse l =
  let description = "ill-formed associativity list does not parse"                   in
  let test _      = l |> entry_of_assoc |> Result.is_error |> assert_bool "is Error" in
  description >:: test

let check_entry l description st tt ss =
  let l_parsed   = Result.get_ok (entry_of_assoc l)                           in
  let st_check _ = assert_equal l_parsed.source_type   st                     in
  let tt_check _ = assert_equal l_parsed.target_type   tt                     in
  let ss_check _ = assert_equal l_parsed.shell_command ss                     in
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

let t_neq_s_transform_data_correct e =
  let description = "entry with source /= target converts to transform data" in
  let td          = transform_data_of_entry e                                in
  let tt _        = assert_equal td.target_type   "a"                        in
  let ss _        = assert_equal td.shell_command "b"                        in
  let v  _        = assert_equal td.variety       DataAndHeader              in
  description >:::
    [ "target type ok" >:: tt ;
      "script ok"      >:: ss ;
      "variety ok"     >:: v  ;
    ]

let tests = "test suite for config file parsing" >:::
  [ parses   wf              ;
    parses   extra           ;
    no_parse nwf             ;
    wf_correct               ;
    extra_correct            ;
    wf_trans_data_correct    ;
    extra_trans_data_correct ;
  ]

let _ = run_test_tt_main tests
