
(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * %%NAME%% %%VERSION%%
 *)

open Prelude
open Cmdliner

type cmd_input = [`Stdin | `File of string]
type cmd_input_parser = string -> (cmd_input, [`Msg of string]) Stdlib.result
type cmd_input_printer = cmd_input Arg.printer

(*(string -> ('a, [ `Msg of string ]) Stdlib.result*)
let cmd_input_parser str = 
  if Sys.file_exists str 
  then Ok (`File str)
  else Error (`Msg ("File: " ^ str ^ " does not exist."))

(*Format.formatter -> `a -> unit*)
let cmd_input_printer fmt input = let str = 
  match input with
    | `Stdin -> "STDIN"
    | `File fn -> fn
  in Format.pp_print_string fmt str

(*parsed_in conv*)
let cmd_input_conv = Arg.conv (cmd_input_parser, cmd_input_printer)

let report ?(params = false) inp =
  let open Lib.Report                                  in
  let module M =  Map.Make(String)                     in
  let types    = match inp with
    | `File fn -> within (content_types ~params:params) fn
    | _ -> content_types ~params:params stdin          in
  print "Content Types:";
  M.iter
    (fun k v -> print ("  " ^ k ^ " : " ^ (Int.to_string v)))
    types

let default_config_name = ".default-config"

let convert inp =
  let open Lib.Convert.Conversion_ocamlnet in
  let open Lib.Configuration.ParseConfig   in
  let open Lib.ErrorHandling               in
  if   Sys.file_exists default_config_name
  then let converted_email =
         let  ( let* ) = Result.(>>=)                          in
         let  input    = match inp with
          | `File fn -> readfile fn
          | _ -> read stdin                                    in
         let* config   = parse_config_file default_config_name in
         acopy_email config input
       in
       match converted_email with
       | Error err    -> print (Error.message err)
       | Ok converted -> write stdout converted
  else Printf.printf "Error: missing config file '%s'\n" default_config_name

(*params matched first in the case both report and params are true*)
let convert_wrapper rpt rpt_p inp =
  if rpt_p then report ~params:true inp
  else if rpt then report inp
  else convert inp

let input_t = let doc = "Input file to be converted." in
Arg.(value & pos 0 cmd_input_conv `Stdin & info [] ~doc)

let report_params_t = let doc = "Prints a list of all MIME types in the input along with 
all header and field parameters that go with it." in
Arg.(value & flag & info ["report-params"] ~doc)

let report_t = let doc = "Provides a list of all attachment types in a given mailbox." in
Arg.(value & flag & info ["r"; "report"] ~doc)

let convert_t = Term.(const convert_wrapper $ report_t $ report_params_t $ input_t)

let cmd = let doc = "Converts email attachments." in
  let info = Cmd.info "attachment-converter" ~doc in
  Cmd.v info convert_t

let main () = exit (Cmd.eval cmd)

let () = main ()
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

