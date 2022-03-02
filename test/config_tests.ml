(* tests *)

open OUnit2
open Lib.Config.ParseConfig

let wf = [
  ("source_type", "application/pdf");
  ("target_type", "application/pdf");
  ("shell_script", "soffice-to-pdfa.sh")
]

let extra = ("extra", "extra") :: wf

let nwf = [
  ("source_type", "application/pdf");
  ("target_type!", "application/pdf");
  ("shell_script", "soffice-to-pdfa.sh")
]

let t_neq_s = [
  ("source_type", "a");
  ("target_type", "b");
  ("shell_script", "c");
]

let t_neq_s_extra = ("d", "d") :: t_neq_s


let wf_parses l =
  let description = "well-formed associativity list parse is Ok" in
  let test _ = l |> entry_of_assoc |> Result.is_ok |> assert_bool "not Ok"
  in
  description >:: test

let nwf_no_parse l =
  let description = "ill-formed associativity list does not parse" in
  let test _ = l |> entry_of_assoc |> Result.is_error |> assert_bool "is Error"
  in
  description >:: test

let wf_correct l =
  let description = "well-formed associativity list converts to entry" in
  let l_parsed = Result.get_ok (entry_of_assoc l) in
  let st _ = assert_equal l_parsed.source_type "application/pdf" in
  let tt _ = assert_equal l_parsed.target_type "application/pdf" in
  let ss _ = assert_equal l_parsed.shell_script "soffice-to-pdfa.sh"
  in
  description >::: [
    "source type ok" >:: st;
    "target type ok" >:: tt;
    "script ok" >:: ss
  ]

let wf_transform_data_correct e =
  let description = "entry with source = target converts to transform_data" in
  let td = transform_data_of_entry e in
  let tt _ = assert_equal td.target_type "application/pdf"     in
  let ss _ = assert_equal td.shell_script "soffice-to-pdfa.sh" in
  let v  _ = assert_equal td.variety DataOnly
  in
  description >::: [
    "target type ok" >:: tt;
    "script ok"      >:: ss;
    "variety ok"     >:: v;
  ]

let t_neq_s_transform_data_correct e =
  let description = "entr with source /= target converts to transform data" in
  let td          = transform_data_of_entry      e                          in
  let tt _        = assert_equal td.target_type  "a"                        in
  let ss _        = assert_equal td.shell_script "b"                        in
  let v  _        = assert_equal td.variety      DataAndHeader
  in
  description >::: [
    "target type ok" >:: tt ;
    "script ok"      >:: ss ;
    "variety ok"     >:: v  ;
  ]

let tests = "test suite for config file parsing" >::: [
  wf_parses wf                                                     ;
  wf_parses extra                                                  ;
  wf_correct wf                                                    ;
  wf_correct extra                                                 ;
  nwf_no_parse nwf                                                 ;
  wf_transform_data_correct (Result.get_ok (entry_of_assoc wf)   ) ;
  wf_transform_data_correct (Result.get_ok (entry_of_assoc extra)) ;
]

let _ = run_test_tt_main tests
