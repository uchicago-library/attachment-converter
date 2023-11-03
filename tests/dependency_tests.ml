open OUnit2
open Lib.Dependency
open Utils 
open Prelude.Prereq

let getPackage_test = 
   check_eq_basic 
   "check for return of correct package" 
   (linux) 
   (getPackage (Linux linux))

let getPkgFromExec_test = 
   check_eq_basic
   "check for return of corresponding package name" 
   ("libreoffice") 
   (getPkgFromExec "soffice")

let printMissingPkgs_test1 = 
   check_eq_basic
   "check does nothing when all dependencies met" 
   ()
   (printMissingPkgs [])

let checkForExecutables_test1 = 
   check_eq_basic 
   "check that an empty list is returned when no executables / packages are missing"
   ([])
   (checkForExecutables (Linux linux))

let getPltfrm_test1 = 
   check_eq_basic
   "check that correct platform and package is assigned if a supported platform" (*assuming a linux machine*)
   (Linux linux)
   (getPltfrm) (*need to find a way to check output of command on all four platforms*)

let checkDependencies_test = (*if getPltfrm, checkforexec, and printmissing all work then this should as well*)
   check_eq_basic
   "check that nothing happens when run on a machine where all dependencies are met"
   ()
   (checkDependencies)

let tests = 
   "test suite for dependency" >:::
   [
      getPackage_test;
      getPkgFromExec_test;
      printMissingPkgs_test1;
      "check that exception is raised when packages are missing" >:: (fun _ -> assert_raises (Failure "libvips, ghostscript still need to be installed") (fun () -> printMissingPkgs ["vips"; "gs"])); (*OK on my machine*)
      "check for return of correct executables" >:: (fun _ -> assert_equal [Exists "soffice"; Exists "pandoc"; Exists "vips"; Exists "gs"] (getExecutables (Linux linux)));
      checkForExecutables_test1;
      getPltfrm_test1;
      checkDependencies_test;
      (* "check that unsupported os triggers exception" >:: (fun _ -> assert_raises (Failure " is not a supported platform") (fun () -> getPltfrm)); *) (*only works if hardcode bad input into getPltfrm*)
   ]
   
let _ = run_test_tt_main tests