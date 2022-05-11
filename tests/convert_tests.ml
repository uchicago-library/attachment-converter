open OUnit2
open Lib.Convert.Conversion_ocamlnet
open Prelude

let empty_config_test_0 tree =
  let description = "empty config is okay on email" in
  let check _ = assert_bool "is okay"
    (Result.good (acopy
      Lib.Configuration.Formats.Dict.empty
      tree))
  in
  description >:: check

let empty_config_test_1 tree =
  let description = "empty config is noop" in
  let check _ = assert_equal
    (Ok tree)
    (acopy
      Lib.Configuration.Formats.Dict.empty
      tree)
  in
  description >:: check

let noop_pdf_config =
  let open Lib.Configuration.Formats in
  Dict.add
    "application/pdf"
    [{ target_type   = "application/pdf" ;
      shell_command = "cat"              ;
      variety       = DataOnly           ;
    }]
    Dict.empty

let test_noop_transform =
  let open Lib.Configuration.Formats                                in
  let open Netmime                                                  in
  let h = new basic_mime_header ["content-type", "application/pdf"] in
  let bd = new memory_mime_body "This is nothing"                   in
  let config = hd (Dict.find "application/pdf" noop_pdf_config)     in
  let description = "transform with cat is noop"                    in
  let check _ = assert_equal
    (h, `Body bd)
    (transform h bd config)
  in
  description >:: check

(* This is a very dumb printing function *)
let print_tree_structure tree =
  let rec print_tree_list tree_list =
    match tree_list with
    | [] -> ""
    | (_, `Body _) :: xs -> "(body_header, body):" ^ print_tree_list xs
    | (_, `Parts parts) :: xs -> "(parts_header, (" ^ print_tree_list parts ^ "):" ^ print_tree_list xs
    in
    print_tree_list [tree]



let noop_pdf_config_test_0 tree =
  let description = "noop config is noop" in
  let check _ = assert_equal
    (Ok tree)
    (amap
      noop_pdf_config
      tree)
    ~printer:(fun x -> print_tree_structure (Result.get_ok x))
  in
  description >:: check

let basic_test_email fname =
  let email = readfile fname in
  "basic test suite for email: " ^ fname >:::
    [ empty_config_test_0    (parse email) ;
      empty_config_test_1    (parse email) ;
      noop_pdf_config_test_0 (parse email) ;
    ]

let tests =
  "test suite for conversion" >:::
    [ basic_test_email "test_emails/xmas_email" ;
      test_noop_transform                       ;
    ]

let _ = run_test_tt_main tests
