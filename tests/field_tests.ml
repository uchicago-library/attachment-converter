(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open OUnit2
open Utils
module F = Lib.Header.Field
module V = F.Value
module P = V.Parameter

let str1 = "Test: test;\r\n\ta1=v1;\r\n\ta2=\"v2\""

let fld1 =
  F.make "Test"
    (V.make
       ~params:
         [ P.make "a1" "v1"; P.make ~quotes:true "a2" "v2" ]
       "test" )

let str2 = " Test : test ; \r\n \ta1=v1; \r\n\ta2=\"v2\""

let of_string_test1 =
  check_eq_basic "basic of_string test" fld1
    (F.of_string str1)

let of_string_test3 =
  check_eq_basic "of_string with spaces test" fld1
    (F.of_string str2)

let to_string_test1 =
  check_eq_string "basic to_string test" str1
    (F.to_string fld1)

let to_assoc_test1 =
  check_eq_basic "basic to_assoc test"
    ("Test", "test;\r\n\ta1=v1;\r\n\ta2=\"v2\"")
    (F.to_assoc fld1)

let tests =
  "test suite for fields"
  >::: [ of_string_test1;
         of_string_test3;
         to_string_test1;
         to_assoc_test1
       ]

let _ = run_test_tt_main tests


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
