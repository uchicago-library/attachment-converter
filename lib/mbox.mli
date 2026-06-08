(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


exception Invalid_mbox

type t

type email =
  { from_line : string;
    from_line_num : int;
    after_from_line : string
  }

val of_in_channel : in_channel -> (t, Error.t) result
val of_in_channel_exn : in_channel -> t
val input_email : t -> email option
val close : t -> unit
val foldl : (email -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (email -> unit) -> t -> unit


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
