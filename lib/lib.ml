(* attachment-converter
 * attachment-converter.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
*)

open Prelude

(* library code for attachment converter goes here *)
module type CONVERT =
sig
  type filepath
  type parsetree
  val parse : string -> parsetree
  val amap : ('a -> 'b) -> parsetree -> parsetree
  val acopy : ('a -> 'b) -> parsetree -> parsetree
  val to_string : parsetree -> string
  val convert : filepath -> string -> string
  val acopy_email : string -> (string -> string) -> string
end

module Conversion_ocamlnet : CONVERT = struct
  include Netmime
  include Netmime_channels
  include Netchannels
  include Netstream

  type filepath = string (* String.t *)
  type parsetree = Netmime.complex_mime_message

  let parse s =
    Netchannels.with_in_obj_channel (new Netstream.input_stream (new Netchannels.input_string s))
      Netmime_channels.read_mime_message
  (* see http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and the Netstream input bits,
    but it's worth keeping an eye on. *)




  let amap = assert false
  let acopy = assert false
  let to_string = assert false
  let convert = assert false
  let acopy_email = assert false
end


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

