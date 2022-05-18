open OUnit2
open Lib.Convert.Conversion_ocamlnet
open Prelude

let empty_config_test_0 tree =
  let description = "empty config is okay on email"     in
  let open Lib.Configuration.Formats                    in
  let is_okay     = Result.good (acopy Dict.empty tree) in
  let check _     = assert_bool "is okay" is_okay       in
  description >:: check

let empty_config_test_1 tree =
  let description = "empty config is noop"        in
  let open Lib.Configuration.Formats              in
  let copied      = acopy Dict.empty tree         in
  let check _     = assert_equal (Ok tree) copied in
  description >:: check

let noop_gif_config =
  let open Lib.Configuration.Formats in
  Dict.add
    "image/gif"
    [{ target_type  = "image/tiff" ;
      shell_command = "cat"        ;
      variety       = DataOnly     ;
    }]
    Dict.empty

let header_change_gif_config =
  let open Lib.Configuration.Formats in
  Dict.add
    "image/gif"
    [{ target_type  = "image/tiff"  ;
      shell_command = "cat"         ;
      variety       = DataAndHeader ;
    }]
    Dict.empty

(* equality predicate for parse trees *)
let rec parsetree_eq t1 t2 =
  let mime_header_eq h1 h2 = h1 # fields = h2 # fields in
  let mime_body_eq   b1 b2 = b1 # value  = b2 # value  in
  match t1, t2 with
  | (h1, `Body b1) , (h2, `Body b2)  -> mime_header_eq h1 h2 && mime_body_eq b1 b2
  | (h1, `Parts p1), (h2, `Parts p2) ->
      mime_header_eq h1 h2 && List.anded (List.zipwith parsetree_eq p1 p2)
  | _, _ -> false

let assert_equal_trees t1 t2 =
  assert_bool "same trees" (parsetree_eq t1 t2)

let test_noop_transform =
  let open Lib.Configuration.Formats                                    in
  let open Netmime                                                      in
  let description = "transform with cat is noop"                        in
  let header      = new basic_mime_header ["content-type", "image/gif"] in
  let body        = new memory_mime_body "This is nothing"              in
  let config      = hd (Dict.find "image/gif" noop_gif_config)          in
  let trans       = transform header body config                        in
  let check _     = assert_equal_trees (header, `Body body) trans       in
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

let noop_gif_config_test_0 tree =
  let description = "noop config is noop" in
  let check _ = assert_equal_trees
    tree
    (Result.get_ok
      (amap noop_gif_config tree))
  in
  description >:: check

let header_change_gif_config_test_0 tree =
  let description = "header change config is not noop" in
  let check _ = assert_equal_trees
    tree
    (Result.get_ok
      (amap header_change_gif_config tree))
  in
  description >:: check


let basic_test_email fname =
  let email = readfile fname in
  "basic test suite for email: " ^ fname >:::
    [ empty_config_test_0    (parse email) ;
      empty_config_test_1    (parse email) ;
      noop_gif_config_test_0 (parse email) ;
    ]

(* add emails as necessary *)
let basic_test_email_all =
  "basic tests for many emails" >:::
    [ (* basic_test_email "test_emails/10-BURNETTA" *) ]

let tests =
  "all tests" >:::
    [ basic_test_email_all   ;
      test_noop_transform    ;
    ]

let _ = run_test_tt_main tests
