open Lib

let _type = Alcotest.of_pp Mime_type.Type.pp
let _subtype = Alcotest.of_pp Mime_type.Subtype.pp

let test1 () =
  let open Mime_type in
  let msg = "Mime type case insensitivity" in
  let ex = Type.application in
  let rl = Type.of_string "ApplICATion" in
  Alcotest.(check _type) msg ex rl

let test2 () =
  let open Mime_type in
  let msg = "Mime subtype case insensitivity" in
  let ex = Subtype.plain in
  let rl = Subtype.of_string "PlAIn" in
  Alcotest.(check _subtype) msg ex rl

(* TODO *)

let () =
  let open Alcotest in
  run "mime-types"
    [ ( "type",
        [ test_case "type case insensitivity" `Quick test1 ]
      );
      ( "subtype",
        [ test_case "subtype case insensitivity" `Quick
            test2
        ] )
    ]
