(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open Prelude.Prereq
open Package
module Trace = Error.T
module E = Dependency_error

let getUserOS () =
  let open Prelude.Unix.Shell in
  let userPltfrm =
    snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
  in
  match userPltfrm with
  | "Linux" -> Ok Package.linux
  | "Darwin" -> Ok Package.darwin
  | _ -> Trace.throw (E.Smart.unsupported_os_err userPltfrm)

let checkExecutables pkgs =
  let rec checkExecutables' (pkgs : Package.t list) acc =
    match pkgs with
    | [] -> acc
    | p :: t -> (
      match check [ p.executable ] with
      | Ok _ :: _ -> checkExecutables' t acc
      | Error _ :: _ -> checkExecutables' t (acc @ [ p ])
      | [] -> [] )
  in
  let results = checkExecutables' pkgs [] in
  match results with
  | [] -> Ok ()
  | h :: t ->
    Trace.throw (E.Smart.not_installed_err (h :: t))

let checkDependencies () =
  let open Prelude.Result in
  let ( let* ) = ( >>= ) in
  let* userPckg = getUserOS () in
  checkExecutables userPckg


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
