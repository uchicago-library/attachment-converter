(*
  2022-04-06, Yoinked by Nathan Mull verbatim, including header
 *)

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

open Prelude

module type SimpleIterator =
sig
  type s
  type t
  type o
  val  opens : s -> t
  val  close : t -> unit
  val  next  : t -> o
  exception End_of_input
end

module FileIterator : SimpleIterator = struct
  type s = string (* filename *)
  type t = { zipped : bool       ;
             ic     : in_channel ;
           }
  type o = string

  let opens filename =
    if   Filename.check_suffix filename ".gz"
    then { ic     = Unix.open_process_in ("gunzip -c " ^filename) ;
           zipped = true                                          ;
         }
    else { ic     = open_in filename                              ;
           zipped = false                                         ;
         }

  let close t =
    if   t.zipped
    then ignore(Unix.close_process_in t.ic)
    else close_in t.ic

  exception End_of_input

  let next t =
    try
      input_line t.ic
    with End_of_file ->
      raise End_of_input
end

module LineIterator : SimpleIterator
  with type s = string
  with type o = string
  = struct
  type s = string (* mbox body *)
  type t = { mutable lines : string list }
  type o = string

  let opens s = { lines = String.split ~sep:"\n" s }
  let close _ = ()

  exception End_of_input

  let next t =
    try
      let next_line = hd t.lines in begin
        t.lines <- tl t.lines;
        next_line
      end
    with Failure _ ->
      raise End_of_input
end

module MBoxIterator
  ( L : SimpleIterator with type o = string )
  : SimpleIterator
  with type s = L.s
  with type o = string * string
  = struct
  type s = L.s
  type t = { input         : L.t      ;
             mutable start : string   ;
             buf           : Buffer.t ;
           }
  type o = string * string

  let opens s = { input = L.opens s           ;
                  start = ""                  ;
                  buf   = Buffer.create 50000 ;
                }

  let close t = L.close t.input

  exception End_of_input

  let next t =
    Buffer.clear t.buf;
    let rec read () =
      let line = L.next t.input in
      if   String.length line >= 5
           && String.sub line 0 5 = "From "
      then let fromline = t.start in begin
             t.start <- line ^ "\n";
             if   Buffer.length t.buf > 0
             then (fromline, Buffer.contents t.buf)
             else read ()
           end
      else begin
             Buffer.add_string t.buf line;
             Buffer.add_char t.buf '\n';
             read ()
           end
    in
    try
      read ()
    with L.End_of_input ->
      if   Buffer.length t.buf > 0
      then let from_line = t.start in begin
           t.start <- "";
           (from_line, Buffer.contents t.buf)
           end
      else raise End_of_input
end



(*
  let mbox_file_iter fn =
    let t = opens () in
      try
        while true do fn (read_msg t) done
      with L.End_of_input ->
        close t

  let mbox_convert outchan fn =
    let ic = opens () in
    let rec loop () =
      match try Some (read_msg ic) with L.End_of_input -> None with
      | Some msg ->
          (match (fn msg) with
           | Ok msg -> loop (write outchan (ic.start ^ "\n" ^ msg))
           | _      -> loop (write outchan (ic.start ^ "\n" ^ msg))) (* TODO: logging *)
      | None -> ()
    in
      Ok (loop ())

  (** [mbox_in_chan convert]: apply [fn] to each message in the mbox open on [inchan], write to
      string, leaving the message unchanged if [fn] fails.
   *)
(*
  let mbox_in_chan_convert ?(eol="\n") inchan fn =    (* Nathan *)
    let ic = open_mbox_channel inchan in
    let rec loop acc =
      match try Some (read_msg ic) with End_of_file -> None with
      | Some msg ->
          (match (fn msg) with
           | Ok msg -> loop (acc ^ eol ^ ic.start ^ eol ^ msg)
           | _      -> loop (acc ^ eol ^ ic.start ^ eol ^ msg)) (* TODO: logging *)
      | None -> ()
    in
      Ok (loop "")
*)
end
*)

