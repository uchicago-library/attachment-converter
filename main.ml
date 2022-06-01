
(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * %%NAME%% %%VERSION%%
 *)

open Prelude

let report ?(params=false) () =
  let open Lib.Report                                  in
  let module M =  Map.Make(String)                     in
  let types    = content_types ~params:params stdin    in
  print "Content Types:";
  M.iter
    (fun k v -> print ("  " ^ k ^ " : " ^ (Int.to_string v)))
    types

let default_config_name = ".config"

let convert ?(single_email=false) () =
  let open Lib.Convert.Conversion_ocamlnet in
  let open Lib.Configuration.ParseConfig   in
  let open Lib.ErrorHandling               in
  if   Sys.file_exists default_config_name
  then let converted =
         let  ( let* ) = Result.(>>=)                                     in
         let  input    = read stdin                                       in
         let* config   = parse_config_file default_config_name            in
         let  conv     = if single_email then acopy_email else acopy_mbox in
         conv config input
       in
       match converted with
       | Error err    -> print (Error.message err)
       | Ok converted -> write stdout converted
  else
    write
      stderr
      (Printf.sprintf
        "Error: missing config file '%s'\n"
        default_config_name)

(* A _very_ minimal executable *)
let main = if   Array.length Sys.argv > 1
           then match Sys.argv.(1) with
                | "--report"        -> report                     ()
                | "--report-params" -> report ~params:true        ()
                | "--single-email"  -> convert ~single_email:true ()
                | unknown_flag      -> write stderr ("unknown flag: " ^ unknown_flag)
           else convert ()

(*
 * Copyright (c) 2021 Matt Teichman
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

