open OUnit2
open Lib.Convert.Conversion_ocamlnet
open Prelude

let new_filename_test_0 =
  let description = "basic new filename test (no star, no time stamp)"  in
  let check _     = assert_equal
    "filename=\"test_CONVERTED.gif\""
    (new_filename ~tstamped:false ~star:false "filename" "test" ".gif")
    ~printer:id                                                         in
  description >:: check

let new_filename_test_1 =
  let description = "basic new filename test (with star, no time stamp)" in
  let check _     = assert_equal
    "filename*=test_CONVERTED.gif"
    (new_filename ~tstamped:false ~star:true "filename" "test" ".gif")
    ~printer:id                                                          in
  description >:: check

let update_filename_string_test_0 =
  let description = "basic filename conversion test (no star, gif to tiff)" in
  let check _     = assert_equal
    "filename=\"test_CONVERTED.tiff\""
    (update_filename_string
      ~tstamped:false
      ".tiff"
      "filename=test.gif")
    ~printer:id                                                             in
  description >:: check

let update_filename_string_test_1 =
  let description = "basic filename conversion test (with star, gif to tiff)" in
  let check _     = assert_equal
    "filename*=test_CONVERTED.tiff"
    (update_filename_string
      ~star:true
      ~tstamped:false
      ".tiff"
      "filename*=test.gif")
    ~printer:id                                                               in
  description >:: check

let basic_cont_dis =
"attachment;    filename*=utf-8''test.gif;    filename=\"test.gif\""

let basic_cont_dis_conv_0 =
"attachment;    filename*=utf-8''test_CONVERTED.tiff;    filename=\"test.gif\""

let basic_cont_dis_conv_1 =
"attachment;    filename*=utf-8''test.gif;    filename=\"test_CONVERTED.tiff\""

let basic_cont_dis_conv_2 =
"attachment;    filename*=utf-8''test_CONVERTED.tiff;    filename=\"test_CONVERTED.tiff\""

let basic_cont_dis_1 =
"attachment;    filename=\"test.gif\";    filename*=utf-8''test.gif"

let basic_cont_dis_conv_3 =
"attachment;    filename=\"test_CONVERTED.tiff\";    filename*=utf-8''test_CONVERTED.tiff"

let split_on_sep_tests =
  let open HeaderValue in
  let f = split_on_sep in
  let t d a b = d >:: (fun _ -> assert_equal a b) in
  "split_on_sep tests" >:::
    [ t "simple test" (f [1;2;3;4;5] [3;4]) ([1;2], [5]) ;
      t "simple empty" (f [1;2;3;4;5] [6]) ([1;2;3;4;5], []);
      t "simple empty first" (f [1;1;1;1;1] [1;1]) ([], [1;1;1]);
      t "simple duplicate" (f [1;2;3;1;2;3] [2;3]) ([1], [1;2;3]);
    ]

let opt_split_on_sep_tests =
  let open HeaderValue in
  let f = opt_split_on_sep in
  let t d a b = d >:: (fun _ -> assert_equal a b) in
  "opt_split_on_sep tests" >:::
    [ t "simple test" (f [1;2;3;4;5] [3;4]) (Some ([1;2], [5])) ;
      t "simple empty" (f [1;2;3;4;5] [6]) None;
      t "simple empty first" (f [1;1;1;1;1] [1;1]) (Some ([], [1;1;1]));
      t "simple duplicate" (f [1;2;3;1;2;3] [2;3]) (Some ([1], [1;2;3]));
    ]

let opt_split_on_sep_str_tests =
  let open HeaderValue in
  let f = opt_split_on_sep_str in
  let t d a b = d >:: (fun _ -> assert_equal a b) in
  "opt_split_on_sep tests" >:::
    [ t "simple test" (f "abcde" "cd") (Some ("ab", "e")) ;
      t "simple empty" (f "abcde" "f") None;
      t "simple empty first" (f "aaaaa" "aa") (Some ("", "aaa"));
      t "simple duplicate" (f "abcabc" "bc") (Some ("a", "abc"));
    ]

let parse_test_1 =
  let open HeaderValue in
  let description = "parsing basic_cont_dis" in
  let param1 = { attr = "filename*"; value = "utf-8''test.gif"; quoted = false } in
  let param2 = { attr = "filename"; value = "test.gif"; quoted = true } in
  let out = Some { head = "attachment"; params = [param1 ; param2 ] } in
  let check _ = assert_equal out (parse basic_cont_dis) in
    description >:: check

let to_string_test_1 =
  let open HeaderValue in
  let description = "simple to_string of basic_cont_dis test" in
  let out = "attachment;\n\tfilename*=utf-8''test.gif;\n\tfilename=\"test.gif\"" in
  let check _ = assert_equal out (to_string (Option.get (parse basic_cont_dis))) ~printer:id in
    description >:: check

let to_string_test_2 =
  let open HeaderValue in
  let description = "simple to_string without params" in
  let out = "attachment;" in
  let check _ = assert_equal out (to_string (Option.get (parse "attachment"))) ~printer:id in
    description >:: check

let update_test_1 =
  let open HeaderValue in
  let description = "simple update_param test" in
  let param1 = { attr = "filename*"; value = "TESTING"; quoted = false } in
  let param2 = { attr = "filename"; value = "test.gif"; quoted = true } in
  let out = { head = "attachment"; params = [param1 ; param2 ] } in
  let check _ = assert_equal out (update_param "filename*" "TESTING" (Option.get (parse basic_cont_dis))) in
    description >:: check

let update_test_2 =
  let open HeaderValue in
  let description = "simple update_head test" in
  let param1 = { attr = "filename*"; value = "utf-8''test.gif"; quoted = false } in
  let param2 = { attr = "filename"; value = "test.gif"; quoted = true } in
  let out = { head = "inline"; params = [param1 ; param2 ] } in
  let check _ = assert_equal out (update_head "inline" (Option.get (parse basic_cont_dis))) in
    description >:: check


let header_value_parser_tests =
  "all header_value tests" >:::
    [ split_on_sep_tests ;
      opt_split_on_sep_tests ;
      opt_split_on_sep_str_tests ;
      parse_test_1 ;
      to_string_test_1 ;
      to_string_test_2 ;
      update_test_1 ;
      update_test_2 ;
     ]

let update_filename_test_0 =
  let description = "basic filename update test (with star, gif to tiff)" in
  let check _     = assert_equal
    basic_cont_dis_conv_0
    (update_filename
      ~ext:".tiff"
      ~tstamped:false
      ~star:true
      basic_cont_dis)
    ~printer:id                                                           in
  description >:: check

let update_filename_test_1 =
  let description = "basic filename update test (no star, gif to tiff)" in
  let check _     = assert_equal
    basic_cont_dis_conv_1
    (update_filename
      ~ext:".tiff"
      ~tstamped:false
      ~star:false
      basic_cont_dis)
    ~printer:id                                                         in
  description >:: check

let update_both_filenames_test_0 =
  let description = "basic update both filenames" in
  let check _     = assert_equal
    basic_cont_dis_conv_2
    (update_both_filenames
      ~ext:".tiff"
      ~tstamped:false
      basic_cont_dis)
    ~printer:id
  in
  description >:: check

let update_both_filenames_test_1 =
  let description = "basic update both filenames (reversed order)" in
  let check _     = assert_equal
    basic_cont_dis_conv_3
    (update_both_filenames
      ~ext:".tiff"
      ~tstamped:false
      basic_cont_dis_1)
    ~printer:id
  in
  description >:: check


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
    [{ target_type   = "image/tiff" ;
       target_ext    = ".tiff"      ;
       shell_command = "cat"        ;
       variety       = DataOnly     ;
    }]
    Dict.empty

let header_change_gif_config =
  let open Lib.Configuration.Formats in
  Dict.add
    "image/gif"
    [{ target_type   = "image/tiff"  ;
       target_ext    = ".tiff"       ;
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

let parse_okay email =
  let description = "check parse is okay" in
  let check _     =
    assert_bool
      "Actually not Ok"
      (Result.good (parse email))         in
  description >:: check

let basic_test_email fname =
  let email = readfile fname in
  "basic test suite for email: " ^ fname >:::
    [ parse_okay email                                     ;
      empty_config_test_0    (Result.get_ok (parse email)) ;
      empty_config_test_1    (Result.get_ok (parse email)) ;
      noop_gif_config_test_0 (Result.get_ok (parse email)) ;
    ]

(*
let email_4_structure =
  let email       = readfile "test_emails/test_email_4.eml" in
  let description = "test_email_4 structure"                in
  let check _ = assert_equal
    (Parts [Body ; Parts [Body; Parts [Body; Body]]])
    (parsetree_to_mailtree (Result.get_ok (parse email)))
    ~printer:mailtree_to_string                             in
  description >:: check

(* add emails as necessary *)
let basic_test_email_all =
  "basic tests for many emails" >:::
    [ basic_test_email "test_emails/test_email_1.eml" ;
      basic_test_email "test_emails/test_email_2.eml" ;
      basic_test_email "test_emails/test_email_3.eml" ;
      basic_test_email "test_emails/test_email_4.eml" ;
      basic_test_email "test_emails/test_email_5.eml" ;
      basic_test_email "test_emails/test_email_6.eml" ;
    ]
*)

let tests =
  "all tests" >:::
    [ (* basic_test_email_all          ; *)
      test_noop_transform           ;
      new_filename_test_0           ;
      new_filename_test_1           ;
      update_filename_string_test_0 ;
      update_filename_string_test_1 ;
      update_filename_test_0        ;
      update_filename_test_1        ;
      update_both_filenames_test_0  ;
      update_both_filenames_test_1  ;
      header_value_parser_tests     ;
    ]

let _ = run_test_tt_main tests
