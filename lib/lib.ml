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
    type htransform = string -> string
    type btransform = string -> string
    val parse : string -> parsetree
    val amap : htransform -> btransform -> parsetree -> parsetree
    val acopy : htransform -> btransform -> parsetree -> parsetree
    val to_string : parsetree -> string
    val convert : filepath -> (string -> string)
    val acopy_email : string -> (string -> string) -> string
  end

module Conversion_ocamlnet : CONVERT = struct
  include Netmime
  include Netmime_channels
  include Netchannels
  include Netstream
  include Netmime_header
  include Stdlib.Buffer


  type htransform = string -> string
  type btransform = string -> string
  type filepath =  String.t
  type parsetree = Netmime.complex_mime_message


  let parse (s : string) =
    let ch = (new Netstream.input_stream (new Netchannels.input_string s)) in
    let f = (fun ch -> Netmime_channels.read_mime_message ~multipart_style:`Deep ch) in
    Netchannels.with_in_obj_channel ch f
  (* see http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and the Netstream input bits,
     but it's worth keeping an eye on. *)

  let header_to_string (h : mime_header) =
    let buf = Stdlib.Buffer.create 1024 in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      (fun ch -> Netmime_string.write_header ch (h#fields));
    Stdlib.Buffer.contents buf

  let header_from_string s =
    new basic_mime_header
      (Netchannels.with_in_obj_channel
         (new Netchannels.input_string s)
         (fun ch -> Netmime_string.read_header (new Netstream.input_stream ch)))


  let rec amap f g (tree : parsetree) =
    match tree with
      (header, `Body b) ->
       let h = header_to_string header in
       let h' = f h in
       if String.lowercase_ascii h = String.lowercase_ascii h'
                                                            (* only invoke g (the converting function) if f converts the header *)
       then tree
       else (header_from_string h', `Body (b#set_value (g b#value); b))
    | (header, `Parts p_lst) ->
       (header, `Parts (List.map (amap f g) p_lst))

  let rec acopy f g tree =
    match tree with
      (_, `Body _) -> tree (* TODO double check desired behavior for root messages without attachments *)

    | (header, `Parts p_lst) ->
       let copy_or_skip part = (* NOTE: two of the three cases here are singleton
                                 lists, which might be a code smell.  Worth
                                 reviewing in case there's a cleaner way to
                                 express this, especially since it's always
                                 *exactly* one or two things *)
         match part with
           (header, `Body (b: mime_body)) ->
            let h = header_to_string header in
            let h' = f h in
            if String.lowercase_ascii h = String.lowercase_ascii h'
            then [part]
            else [ (header_from_string h', `Body (b#set_value (g b#value); b)); part]
         | _ -> [acopy f g part]

       in (header, `Parts List.(concat_map copy_or_skip p_lst))



  let to_string (tree : parsetree) =
    let (header, _) = tree in
    let n = try Netmime_header.get_content_length header
            with Not_found -> (1024 * 1024) in (* defaulting to a megabyte seems like a nice round number, might be overkill *)
    let buf =  Stdlib.Buffer.create n in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      (fun ch -> Netmime_channels.write_mime_message ch tree);
    Stdlib.Buffer.contents buf

  (* TODO *)
  let convert _path_to_util = assert false
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

