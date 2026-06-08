(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open OUnit2
open Prelude
open Lib

let check_is_something x name =
  let description = Printf.sprintf "%s is something" name in
  let error_str = Printf.sprintf "actual %s is none" name in
  let test _ =
    x |> Option.something |> assert_bool error_str
  in
  description >:: test

let check_is_ok x name =
  let description = Printf.sprintf "%s is Ok" name in
  let error_str =
    Printf.sprintf "actually %s is not Ok" name
  in
  let test _ = x |> Result.good |> assert_bool error_str in
  description >:: test

let check_is_error x name =
  let description = Printf.sprintf "%s is Error" name in
  let error_str =
    Printf.sprintf "actually %s is not Error" name
  in
  let test _ = x |> Result.bad |> assert_bool error_str in
  description >:: test

let check_eq_basic description expected actual =
  let test _ = assert_equal expected actual in
  description >:: test

let check_eq_string description expected actual =
  let test _ = assert_equal expected actual ~printer:id in
  description >:: test

let skeleton_test skel name =
  let conv =
    Convert.Mrmime_parsetree.of_string
      (Skeleton.to_email skel)
  in
  let test1 = check_is_ok conv name in
  let tests =
    match conv with
    | Ok out ->
      let test2 =
        check_eq_basic "skeleton test" skel
          (Convert.Mrmime_Converter.to_skeleton out)
      in
      [ test1; test2 ]
    | Error _ -> [ test1 ]
  in
  "skeleton tests for " ^ name >::: tests


(* This file is part of Attachment Converter. *)

(* Attachment Converter is free software: you can redistribute it *)
(* and/or modify it under the terms of the GNU General Public License *)
(* as published by the Free Software Foundation, either version 3 of *)
(* the License, or (at your option) any later version. *)

(* Attachment Converter is distributed in the hope that it will be *)
(* useful, but WITHOUT ANY WARRANTY; without even the implied warranty *)
(* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Attachment Converter. If not, see *)
(* <https://www.gnu.org/licenses/>. *)
