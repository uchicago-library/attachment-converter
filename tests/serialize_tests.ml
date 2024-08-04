open OUnit2
open Utils
open Lib

let basic_skel1 =
  Skeleton.(Multipart [ Some Body; Some dummy_attachment ])

let basic_skel2 =
  Skeleton.(
    Multipart
      [ Some Body;
        Some Body;
        Some dummy_attachment;
        Some
          (Message
             (Message
                (Multipart
                   [ Some Body; Some dummy_attachment ] ) )
          );
        Some dummy_attachment
      ] )

let tests =
  "test suite for serialization"
  >::: [ skeleton_test basic_skel1 "basic_skel1";
         skeleton_test basic_skel2 "basic_skel2"
       ]

let _ = run_test_tt_main tests
