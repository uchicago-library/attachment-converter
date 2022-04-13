(* 2022-04-06, Yoinked by Nathan Mull verbatim, including header *)

(** {1 Mbox parser ({i Xavier Leroy})}

  Snarfed from: <{{:http://cristal.inria.fr/~xleroy/software.html#spamoracle}http://cristal.inria.fr/~xleroy/software.html#spamoracle}>

  Hacked by KW 2010-05-13 <{{:http://www.lib.uchicago.edu/keith/}http://www.lib.uchicago.edu/keith/}>
    - added map and fold functionals

  @author Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *)
(***********************************************************************)
(*                                                                     *)
(*                 SpamOracle -- a Bayesian spam filter                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  This file is distributed under the terms of the   *)
(*  GNU Public License version 2, http://www.gnu.org/licenses/gpl.txt  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mbox.ml,v 1.4 2002/08/26 09:35:25 xleroy Exp $ *)

(* Reading of a mailbox file and splitting into individual messages *)

type t =
  { ic: in_channel;
    zipped: bool;
    mutable start: string;
    buf: Buffer.t }

let open_mbox_file filename =
  if Filename.check_suffix filename ".gz" then
    { ic = Unix.open_process_in ("gunzip -c " ^filename);
      zipped = true;
      start = "";
      buf = Buffer.create 50000 }
  else
    { ic = open_in filename;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let open_mbox_channel ic =
    { ic = ic;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let read_msg t =
  Buffer.clear t.buf;
  Buffer.add_string t.buf t.start;
  let rec read () =
    let line = input_line t.ic in
    if String.length line >= 5
    && String.sub line 0 5 = "From "
    && Buffer.length t.buf > 0 then begin
      t.start <- (line ^ "\n");
      Buffer.contents t.buf
    end else begin
      Buffer.add_string t.buf line;
      Buffer.add_char t.buf '\n';
      read ()
    end in
  try
    read()
  with End_of_file ->
    if Buffer.length t.buf > 0 then begin
      t.start <- "";
      Buffer.contents t.buf
    end else
      raise End_of_file

let close_mbox t =
  if t.zipped
  then ignore(Unix.close_process_in t.ic)
  else close_in t.ic

let mbox_file_iter filename fn =
  let ic = open_mbox_file filename in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

(** [mbox_convert convert]: [convert] each message in the mbox open on stdin, put to stdout, leaving the message unchanged if [convert] fails. *)
let mbox_convert convert =    (* Nathan *)
  let ic = open_mbox_channel stdin in
  let rec loop =
    match try Some (read_msg ic) with End_of_file -> None with
    | Some msg ->
        (match (convert msg) with
         | Ok msg -> print (ic.start ^ msg)
         | _      -> print (ic.start ^ msg)) (* TODO: logging? *)
    | None -> ()
  in
  loop

(** [mbox_file_fold fn inchan acc]: fold the function [fn] over the messages in the mbox file open on [inchan] with [acc] as initial accumulator. *)
let mbox_file_fold fn inchan acc =		(* KW *)
  let ic = open_mbox_channel inchan in
  let rec loop acc =
    match try Some (read_msg ic) with End_of_file -> None with
      | Some msg -> loop (fn acc msg)
      | None     -> acc
  in
    loop acc

(** [mbox_file_map fn filename]: map the function [fn] over the messages in the mbox file [filename]. *)
let mbox_file_map fn filename =		(* KW *)
  let ic = open_in filename in
    try
      let result = List.rev (mbox_file_fold (fun acc msg -> fn msg::acc) ic []) in
	close_in ic;
	result
    with exn -> close_in ic; raise exn

let mbox_channel_iter inchan fn =
  let ic = open_mbox_channel inchan in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

let read_single_msg inchan =
  let res = Buffer.create 10000 in
  let buf = Bytes.create 1024 in
  let rec read () =
    let n = input inchan buf 0 (Bytes.length buf) in
    if n > 0 then begin
      Buffer.add_subbytes res buf 0 n;
      read ()
    end in
  read ();
  Buffer.contents res
