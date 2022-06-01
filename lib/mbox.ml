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

module type ITERATOR =
  sig
    type s
    type t
    type o
    val  opens : s -> t
    val  close : t -> unit
    val  next  : t -> o
    exception End_of_input
  end

module type LOG =
  sig
    type s
    type t
    type i
    type o
    val opens : s -> t
    val close : t -> unit
    val write : t -> i -> unit
    val value : t -> o
  end

module StringLog : LOG
  with type s = unit
  with type i = string
  with type o = string
  = struct
  type s = unit
  type t = { mutable curr : string }
  type i = string
  type o = string

  let opens () = { curr = "" }
  let close _  = ()
  let write t str = t.curr <- t.curr ^ str
  let value t = t.curr
end

module StdOutLog : LOG = struct
  type s = unit
  type t = unit
  type i = string
  type o = unit

  let opens () = ()
  let close _  = ()
  let write _  = print
  let value _  = ()
end

module FileIterator : ITERATOR
  with type s = string
  with type o = string
  = struct
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

module LineIterator : ITERATOR
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

module StdInIterator : ITERATOR
  with type s = unit
  with type o = string
  = struct
  type s = unit
  type t = unit
  type o = string

  let opens () = ()
  let close () = ()

  exception End_of_input

  let next () =
    try
      read_line ()
    with End_of_file ->
      raise End_of_input
end

module MBoxIterator
  ( L : ITERATOR with type o = string )
  : ITERATOR
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
             t.start <- line;
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

module IteratorFunctions (I : ITERATOR) (L : LOG) = struct

  include I

  let iter s fn =
    let t = I.opens s in
      try
        while true do fn (I.next t) done
      with I.End_of_input ->
        I.close t

  let convert s log_s fn =
    let t   = I.opens s in
    let log = L.opens log_s in
    let rec loop () =
      match try Some (fn (I.next t)) with I.End_of_input -> None with
      | Some converted -> L.write log converted; loop ()
      | None -> ()
    in begin
      loop ();
      I.close t;
      L.close log;
      L.value log;
    end
end
