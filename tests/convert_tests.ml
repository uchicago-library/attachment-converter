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
    "image/gif"
    [{ target_type   = "image/tiff" ;
      shell_command = "cat"              ;
      variety       = DataOnly           ;
    }]
    Dict.empty

let test_noop_transform =
  let open Lib.Configuration.Formats                          in
  let open Netmime                                            in
  let h = new basic_mime_header ["content-type", "image/gif"] in
  let bd = new memory_mime_body "This is nothing"             in
  let config = hd (Dict.find "image/gif" noop_pdf_config)     in
  let description = "transform with cat is noop"              in
  let check _ = assert_equal
    (h, `Body bd)
    (transform h bd config)
  in
  description >:: check

type mailtree =
  | Body
  | Parts of mailtree list

let rec parsetree_to_mailtree tree =
  match tree with
  | (_, `Body _) -> Body
  | (_, `Parts parts) -> Parts (map parsetree_to_mailtree parts)

let rec mailtree_to_string tree =
  match tree with
  | Body -> "B"
  | Parts xs ->
      "P " ^
      (List.foldl (fun x y -> x ^ (if x = "(" then "" else ", ") ^
      (mailtree_to_string y)) "(" xs)
      ^ ")"

let print_tree_structure tree =
  mailtree_to_string (parsetree_to_mailtree tree)

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

let rec first_n n =
  if n <= 0 then
    []
  else
    n :: first_n (n-1)

let basic_test_email_all =
  "basic tests for many emails" >:::
    [ basic_test_email "test_emails/10-BURNETTA" ]

let tests =
  "all tests" >:::
    [ basic_test_email_all ;
      (* test_noop_transform                  ; *)
    ]

let _ = run_test_tt_main tests
