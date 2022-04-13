
(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * %%NAME%% %%VERSION%%
 *)

open Prelude

let report ?(params = false) () =
  let open Lib.Report                                   in
  let open String                                       in
  let module SS = Set.Make(String)                      in
  let types     = attachment_types ~params:params stdin in
  print "Attachment Types:";
  SS.iter (prepend "  " >> print) types;
  Ok ()

let default_config () =
  Lib.Configuration.ParseConfig.parse_config_file
    ".default-config"

let convert_email () =
  let open Lib.Conversion_ocamlnet                        in
  let  ( let* )  = Result.(>>=)                           in
  let* config    = default_config ()                      in
  let* converted = full_convert_email config (read stdin) in
  Ok (write stdout converted)

let convert_mbox () =
  let open Lib.Conversion_ocamlnet  in
  let  ( let* ) = Result.(>>=)      in
  let* config   = default_config () in
  acopy_mbox config

(* A _very_ minimal executable *)
let main = if   Array.length Sys.argv > 1
           then match Sys.argv.(1) with
                | "--report"        -> report              ()
                | "--report-params" -> report ~params:true ()
                | "--single-email"  -> convert_email       ()
                | unknown_flag      -> Error (`UnknownFlag unknown_flag)
           else convert_mbox ()

let print_error =
  match main with
  | Error err -> write stderr (Lib.Error.message err)
  | _ -> ()

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

