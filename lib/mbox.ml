(** {1 Mbox parser ({i Xavier Leroy})}

  Snarfed from: <{{:http://cristal.inria.fr/~xleroy/software.html#spamoracle}http://cristal.inria.fr/~xleroy/software.html#spamoracle}>

  Hacked by KW 2010-05-13 <{{:http://www.lib.uchicago.edu/keith/}http://www.lib.uchicago.edu/keith/}>
    - added map and fold functionals

  Borrowed by Nathan Mull 2022-04-06 for Attachment Converter

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

module type OUTPUT =
sig
  type s
  type t
  type o
  val create: s -> t
  val write: t -> string -> unit
  val value: t -> o
end

module ChannelOutput
  : OUTPUT
  with type s = out_channel
  with type o = unit
  = struct
  type s = out_channel
  type t = out_channel
  type o = unit

  let create chan = chan
  let write chan str = Prelude.write chan str
  let value _ = ()
end

module StringOutput
  : OUTPUT
  with type s = unit
  with type o = string
  = struct
  type s = unit
  type t = { mutable out : string }
  type o = string

  let create () = { out = "" }
  let write t str = t.out <- t.out ^ str
  let value t = t.out
end

module type INPUT =
sig
  type s
  type t
  type o
  val create: s -> t
  val next: t -> o
  exception End_of_input
end

module ChannelInput
  : INPUT
  with type s = in_channel
  with type o = string
  = struct
  type s = in_channel
  type t = in_channel
  type o = string

  exception End_of_input

  let create chan = chan

  let next chan =
    try
      readline chan
    with End_of_file ->
      raise End_of_input
end

module StringInput
  : INPUT
  with type s = string
  with type o = string
  = struct
  type s = string
  type t = { mutable lines : string list }
  type o = string

  exception End_of_input

  let create s = { lines = String.split ~sep:"\r\n" s }

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
  (I: INPUT with type o = string)
  : INPUT
  with type s = I.s
  with type o = string * string
  = struct
  type s = I.s
  type t =
    { input : I.t ;
      mutable start: string ;
      buf : Buffer.t ;
    }
  type o = string * string

  let create s =
    { input = I.create s ;
      start = "" ;
      buf = Buffer.create 50000 ;
    }

  exception End_of_input

  (* TODO: parameterize next on line break format, e.g. let next
     ?(line_feed:Unix) t = etc. *)
  let next t =
    Buffer.clear t.buf;
    let rec read () =
      let line = I.next t.input in
        if   String.length line >= 5 && String.sub line 0 5 = "From "
        then let fromline = t.start in begin
               t.start <- line;
               if   Buffer.length t.buf > 0
               then (fromline, Buffer.contents t.buf)
               else read ()
             end
        else begin
            Buffer.add_string t.buf line;
            (* TODO: make CRLF parameterizable *)
            (* because this is where we're writing what we're reading
               in to the output buffer, ergo this is for
               pre-processing *)
            (* alternatively, always make this CRLF, and then
               conditionally convert it back on the way out *)
               Buffer.add_string t.buf (eol CRLF);
               read ()
             end
    in
    try
      read ()
    with I.End_of_input ->
      if   Buffer.length t.buf > 0
      then let from_line = t.start in begin
           t.start <- "";
           (from_line, Buffer.contents t.buf)
           end
      else raise End_of_input
end

module Conversion (I: INPUT) (O: OUTPUT) = struct
  let convert si so converter =
    let iterator = I.create si in
    let output   = O.create so in
    let rec loop () =
      match try Some (converter (I.next iterator)) with I.End_of_input -> None with
      | Some converted -> O.write output converted; loop () (* TODO: Better output/logging *)
      | None -> ()
    in
      loop ();
      O.value output
end

module ToOutput = struct
  module Make (T : Convert.PARSETREE) = struct
    let convert_mbox in_chan converter =
      let open Conversion (MBoxIterator (ChannelInput)) (ChannelOutput) in
        convert in_chan stdout converter

    let acopy_mbox ?(idem=true) config in_chan pbar =
      let module C = Convert.Conversion.Make (T) in
      let converter (fromline, em) =
        match C.acopy_email ~idem:idem config em pbar with
        | Ok converted -> fromline ^ "\n" ^ converted
        | Error _ ->
            let open ErrorHandling.Printer in
            print "conversion failure\n"; fromline ^ "\n" ^ em (* TODO: better logging *)
      in
        Ok (convert_mbox in_chan converter)
  end
end

module Copier = ToOutput.Make (Convert.Converter)

(*
 * A simple utility function for reading in emails
 * and replacing newlines
 *)
let read_email ic =
  let buf = Buffer.create 50000 in
  let rec read () =
    Buffer.add_string buf (readline ic);
    Buffer.add_string buf (eol CRLF);
    read ()
  in
  try read () with End_of_file ->
    Buffer.contents buf
