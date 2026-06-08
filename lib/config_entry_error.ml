(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


type t =
  [ `MissingKey of Config_key.t
  | `BadMimeType of string * Config_key.t ]

module Smart = struct
  let missing_key_err k = `MissingKey k
  let bad_mime_err ty k = `BadMimeType (ty, k)
end

let debug e =
  match e with
  | `MissingKey _ -> "Config error: Missing key"
  | `BadMimeType _ -> "Config error: Bad mime type"


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
