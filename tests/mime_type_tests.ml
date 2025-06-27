open Lib

let type_ =
  Alcotest.testable
    Mime_type.Type.pp
    Mime_type.Type.equal

(* let subtype_ = *)
(*   Alcotest.testable *)
(*     Mime_type.Subtype.pp *)
(*     Mime_type.Subtype.equal *)

let mime_type =
  Alcotest.testable
    Mime_type.pp
    Mime_type.equal

let test_type_of_string_case_sensitivity () =
  let msg = "same type" in
  let exp = Some Mime_type.Type.application in
  let act = Result.to_option (Mime_type.Type.of_string "APPlicatioN") in
  Alcotest.(check (option type_)) msg exp act

let test_of_string_case_senitivity () =
  let msg = "same type" in
  let exp = Some Mime_type.pdf in
  let act = Result.to_option (Mime_type.of_string "APPliCATion/PDf") in
  Alcotest.(check (option mime_type)) msg exp act

let test_make () =
  let open Mime_type in
  let msg = "same type" in
  let exp = Result.to_option (Mime_type.of_string "model/mesh") in
  let act =
    let ( let* ) = Result.bind in
    let* ty = Type.of_string "model" in
    let* subty = Subtype.of_string "mesh" in
    Ok (make ty subty)
  in
  let act = Result.to_option act in
  Alcotest.(check (option mime_type)) msg exp act

let () =
  let open Alcotest in
  run "Mime_type"
    [ "Type.of_string",
      [ test_case
          "case insensitive"
          `Quick
          test_type_of_string_case_sensitivity
      ];
      "of_string",
      [ test_case
          "case insensitive"
          `Quick
          test_of_string_case_senitivity
      ]
    ]
