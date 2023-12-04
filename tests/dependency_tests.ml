open OUnit2
open Utils 
open Prelude.Prereq
open Prelude.Unix.Shell
open Lib.Dependency

let getUserOS_test_Linux = 
  let test _ = (skip_if (snd (input Prelude.readline @@ cmd ["uname";"-s"]) = "Darwin") "this test won't run with MaCOS"); 
  assert_equal (Ok Package.linux) (getUserOS ()) in 
  "check that correct package is assigned for Linux machine" >:: test 


let getUserOS_test_Darwin = 
  let test _ = (skip_if (snd (input Prelude.readline @@ cmd ["uname";"-s"]) = "Linux") "this test won't run with Linux"); 
  assert_equal (Ok Package.darwin) (getUserOS ()) in 
  "check that correct package is assigned for Darwin machine" >:: test

let checkExecutables_test1_Linux =
  let test _ =  (skip_if (snd (input Prelude.readline @@ cmd ["uname";"-s"]) = "Darwin") "this test won't run with MaCOS");
  assert_equal (Ok ()) (checkExecutables Package.linux) in 
  "check that checkExecutables returns an empty result is returned when all dependencies are met" >:: test
  
let checkExecutables_test1_Darwin =
  let test _ =  (skip_if (snd (input Prelude.readline @@ cmd ["uname";"-s"]) = "Linux") "this test won't run with Linux");
  assert_equal (Ok ()) (checkExecutables Package.darwin) in 
  "check that checkExecutables returns an empty result is returned when all dependencies are met" >:: test

let checkExecutables_test2 =  
  check_eq_basic
  "check that correct error with packages is returned when executables are missing"
  (Error (`NotInstalled [{Package.app = Verapdf; packageName = "verapdf"; executable = Exists "verapdf"}]))
  (checkExecutables [{app = Verapdf; packageName = "verapdf"; executable = Exists "verapdf"}; {app = Vips; packageName = "libvips"; executable = Exists "vips"}])

let checkDependencies_test = 
  check_eq_basic
  "check that nothing happens when the OS and Dependencies are all good to go"
  (Ok "")
  (checkDependencies ())

let printError_testUnsupported =  
  check_eq_string
  "test that error message for unsupported operating system displays correctly"
  ("BadOS is not a supported operating system for Attachment Converter.\nHere is a list of supported Os-es:\n\n\tmacOS\n\tArch Linux\n\tWSL Debian\n\r")
  (Error.message (`UnsupportedOS "BadOS"))

let printError_testNotInstalled = 
  let open Package in
  check_eq_string
  "check that the error message for uninstalled dependencies displays correctly"
  ("Attachment Converter will not run unless all of its OS-level dependencies are installed.\n\nIt looks like the following software packages still need to be installed:\n\tpandoc\n\tlibvips\n\r")
  (Error.message (`NotInstalled [{app = Pandoc; packageName = "pandoc"; executable = Exists "pandoc"}; {app = Vips; packageName = "libvips"; executable = Exists "vips"}]))

let tests = 
   "test suite for dependency" >:::
   [ 
    getUserOS_test_Linux;
    getUserOS_test_Darwin;
    checkExecutables_test1_Linux;
    checkExecutables_test1_Darwin;
    checkExecutables_test2;
    checkDependencies_test;
    printError_testUnsupported;
    printError_testNotInstalled;
   ]
   
let _ = run_test_tt_main tests