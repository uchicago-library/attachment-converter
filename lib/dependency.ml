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
