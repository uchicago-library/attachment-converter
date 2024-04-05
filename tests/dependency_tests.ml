open OUnit2
open Utils
open Prelude.Prereq
open Prelude.Unix.Shell
open Lib.Dependency

let getUserOS_test_Linux =
  let test _ =
    skip_if
      ( snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
      = "Darwin" )
      "this test won't run with MaCOS" ;
    assert_equal (Ok Package.linux) (getUserOS ())
      ~printer:(fun x ->
        x |> Result.get_ok |> Package.toString )
  in
  "check that correct package is assigned for Linux machine"
  >:: test

let getUserOS_test_Darwin =
  let test _ =
    skip_if
      ( snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
      = "Linux" )
      "this test won't run with Linux" ;
    assert_equal (Ok Package.darwin) (getUserOS ())
      ~printer:(fun x ->
        x |> Result.get_ok |> Package.toString )
  in
  "check that correct package is assigned for Darwin \
   machine"
  >:: test

let checkExecutables_test1_Linux =
  let description =
    Printf.sprintf "%s is Ok"
      "(checkExecutables Package.linux)"
  in
  let error_str =
    Printf.sprintf "actually %s is not Ok"
      "(checkExecutables Package.linux)"
  in
  let open Prelude.Result in
  let test _ =
    skip_if
      ( snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
      = "Darwin" )
      "this test won't run with MaCOS" ;
    checkExecutables Package.linux
    |> good
    |> assert_bool error_str
  in
  description >:: test

let checkExecutables_test1_Darwin =
  let description =
    Printf.sprintf "%s is Ok"
      "(checkExecutables Package.darwin)"
  in
  let error_str =
    Printf.sprintf "actually %s is not Ok"
      "(checkExecutables Package.darwin)"
  in
  let open Prelude.Result in
  let test _ =
    skip_if
      ( snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
      = "Linux" )
      "this test won't run with Linux" ;
    checkExecutables Package.darwin
    |> good
    |> assert_bool error_str
  in
  description >:: test

let checkExecutables_test2 =
  let test _ =
    assert_equal
      (Error
         (`NotInstalled
           [ { Package.app = Verapdf;
               packageName = "verapdf";
               executable = Exists "verapdf"
             }
           ] ) )
      (checkExecutables
         [ { app = Verapdf;
             packageName = "verapdf";
             executable = Exists "verapdf"
           };
           { app = Vips;
             packageName = "libvips";
             executable = Exists "vips"
           }
         ] )
      ~printer:(fun x ->
        match x with
        | Error y -> Error.toString y
        | Ok _ -> "" )
  in
  "check that correct error with packages is returned when \
   executables are missing"
  >:: test

let checkDependencies_test =
  check_is_ok
    (checkDependencies ())
    "(checkDependencies ())"

let printError_testUnsupported =
  check_eq_string
    "test that error message for unsupported operating \
     system displays correctly"
    "BadOS is not a supported operating system for \
     Attachment Converter.\n\
     Here is a list of supported Os-es:\n\n\
     \tmacOS\n\
     \tArch Linux\n\
     \tWSL Debian\n\
     \r"
    (Error.message (`UnsupportedOS "BadOS"))

let printError_testNotInstalled =
  let open Package in
  check_eq_string
    "check that the error message for uninstalled \
     dependencies displays correctly"
    "Attachment Converter will not run unless all of its \
     OS-level dependencies are installed.\n\n\
     It looks like the following software packages still \
     need to be installed:\n\
     \tpandoc\n\
     \tlibvips\n\
     \r"
    (Error.message
       (`NotInstalled
         [ { app = Pandoc;
             packageName = "pandoc";
             executable = Exists "pandoc"
           };
           { app = Vips;
             packageName = "libvips";
             executable = Exists "vips"
           }
         ] ) )

let tests =
  "test suite for dependency"
  >::: [ getUserOS_test_Linux;
         getUserOS_test_Darwin;
         checkExecutables_test1_Linux;
         checkExecutables_test1_Darwin;
         checkExecutables_test2;
         checkDependencies_test;
         printError_testUnsupported;
         printError_testNotInstalled
       ]

let _ = run_test_tt_main tests
