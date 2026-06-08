(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


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
