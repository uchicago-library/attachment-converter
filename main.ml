
(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * %%NAME%% %%VERSION%%
 *)

open Prelude
open Lib.Conversion_ocamlnet
open Lib.Config.ParseConfig

let default_config_name = ".default-config"

(* A minimial executable which converts all attachments in a single email given
   at stdin *)
let () =
  if   Sys.file_exists default_config_name
  then let converted_email =
         let ( let* ) = Result.(>>=) in
         let input = read stdin in
         let* config = parse_config_file default_config_name in
         full_convert_email config input
       in
       match converted_email with
       | Error err -> print (Lib.Error.message err)
       | Ok converted -> write stdout converted
  else print (Printf.sprintf "Error: missing config file '%s'" default_config_name)

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

