open OUnit2
open Utils
open Lib

let basic_skel1 = Skeleton.(Some (Multipart [Some Body ; Some Attachment]))
let basic_skel2 = Skeleton.(
  Some (Multipart
    [ Some Body ;
      Some Body ;
      Some Attachment ;
      Some (Message (Message (Multipart
        [ Some Body ;
          Some Attachment ;
        ]))) ;
      Some Attachment ;
    ])
)

let tests =
  "test suite for serialization" >:::
    [ skeleton_test basic_skel1 "basic_skel1" ;
      skeleton_test basic_skel2 "basic_skel2" ;
    ]

let _ = run_test_tt_main tests
