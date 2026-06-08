(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open Prelude

let message msgs =
  String.join ~sep:"" (map Error.to_string msgs)

let debug es =
  let to_string e =
    match e with
    | #Config_entry_error.t as e ->
      Config_entry_error.debug e
    | #Mime_type_error.t as e -> Mime_type_error.debug e
    | #Formats_error.t as e -> Formats_error.debug e
    | _ -> "TODO"
  in
  String.join ~sep:"\n"
    ([ "Error Trace:" ] @ List.map to_string es)


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
