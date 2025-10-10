(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * %%NAME%% %%VERSION%%
 *)

open Prelude
open Cmdliner

let cmd_input_parser str =
  if Sys.file_exists str
  then Ok (`File str)
  else Error (`Msg ("File: " ^ str ^ " does not exist."))

let cmd_input_printer fmt input =
  let str =
    match input with
    | `Stdin -> "STDIN"
    | `File fn -> fn
  in
  Format.pp_print_string fmt str

let cmd_input_conv =
  Arg.conv (cmd_input_parser, cmd_input_printer)

let report ?(params = false) ic =
  let open Lib.Report in
  let module M = Map.Make (String) in
  let types = content_types ~params ic in
  print "Content Types:" ;
  M.iter
    (fun k v -> print ("  " ^ k ^ " : " ^ Int.to_string v))
    types

let convert config target pbar backend =
  let b =
    let open Lib.Backend in
    match backend with
    | Mrmime ->
      ( module Lib.Convert.Mrmime_Converter
      : Lib.Convert.CONVERTER )
    | Ocamlnet ->
      ( module Lib.Convert.Ocamlnet_Converter
      : Lib.Convert.CONVERTER )
  in
  let module B = (val b) in
  let open B in
  match target with
  | `Email email -> convert_single_email config email pbar
  | `Mbox mbox -> convert_mbox config mbox pbar

let convert_wrapper config_files single_email rpt rpt_p inp
    backend =
  let pbar =
    match open_out "/dev/tty" with
    (* no controlling tty *)
    | exception _ -> open_out "/dev/null"
    | other -> other
  in
  let report_or_convert ic =
    if rpt_p
    then report ~params:true ic
    else if rpt
    then report ic
    else
      let ( let* ) = Result.bind in
      let check_and_convert =
        let* () = Lib.Dependency.checkDependencies () in
        let* config =
          Lib.Configuration.get_config config_files
        in
        let* target =
          if single_email
          then Ok (`Email (In_channel.input_all ic))
          else
            let* mbox = Lib.Mbox.of_in_channel ic in
            Ok (`Mbox mbox)
        in
        Ok (convert config target pbar backend)
      in
      match check_and_convert with
      | Error err ->
        write stderr (Lib.Error_message.message err)
      | Ok _ -> ()
  in
  match inp with
  | `File fn ->
    let ic = open_in fn in
    report_or_convert ic
  | `Stdin -> report_or_convert stdin

let backend_t =
  let open Lib.Backend in
  let doc =
    "Choose between 'ocamlnet' and 'mrmime' as the two \
     possible email parsing backends."
  in
  let docv = "BACKEND" in
  let backends =
    [ ("ocamlnet", Ocamlnet); ("mrmime", Mrmime) ]
  in
  Arg.(
    value
    & opt (enum backends) Ocamlnet
    & info [ "backend" ] ~doc ~docv )

let input_t =
  let doc = "Input file to be converted." in
  Arg.(value & pos 0 cmd_input_conv `Stdin & info [] ~doc)

let report_params_t =
  let doc =
    "Prints a list of all MIME types in the input along with\n\
     all header and field parameters that go with it."
  in
  Arg.(value & flag & info [ "report-params" ] ~doc)

let report_t =
  let doc =
    "Provides a list of all attachment types in a given \
     mailbox."
  in
  Arg.(value & flag & info [ "r"; "report" ] ~doc)

let single_email_t =
  let doc =
    "Converts email attachments assuming the input is a \
     single plain text email."
  in
  Arg.(value & flag & info [ "single-email" ] ~doc)

let config_t =
  let doc =
    "Sets the absolute path $(docv) to be checked for a \
     configuration file."
  in
  Arg.(
    value
    & opt_all file []
    & info [ "config" ] ~docv:"PATH" ~doc )

let convert_t =
  Term.(
    const convert_wrapper
    $ config_t
    $ single_email_t
    $ report_t
    $ report_params_t
    $ input_t
    $ backend_t )

let cmd =
  let doc = "Converts email attachments." in
  let open Lib.Version in
  let info = Cmd.info "attc" ~version:ver_num ~doc in
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
