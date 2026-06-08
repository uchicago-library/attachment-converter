(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open Prelude

(* note: should work on both mbox and individual email *)
let content_types ?(params = false) channel =
  let open String in
  let module M = Map.Make (String) in
  let content_header = "content-type:" in
  let split_index = len content_header in
  let is_ct_line =
    prefix content_header << lowercase_ascii
  in
  let mime_type =
    drop split_index >> trim whitespace >> lowercase_ascii
  in
  let just_mime =
    takewhile (not << contains (whitespace ^ ";"))
  in
  let inc_or_zero k =
    M.update k (fun o ->
        Some
          ( match o with
          | None -> 1
          | Some x -> x + 1 ) )
  in
  let add_to_set curr line =
    let mime =
      if params
      then mime_type line
      else just_mime (mime_type line)
    in
    if is_ct_line line then inc_or_zero mime curr else curr
  in
  Prelude.foldlines add_to_set M.empty channel


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
