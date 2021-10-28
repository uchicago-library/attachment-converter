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
  val amap : ('a -> 'a) -> ('b -> 'b) -> parsetree -> parsetree
  val acopy : ('a -> 'a) -> ('b -> 'b) -> parsetree -> parsetree
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


  type filepath = string (* String.t *)
  type parsetree = Netmime.complex_mime_message


  let parse s =
    let ch = (new Netstream.input_stream (new Netchannels.input_string s)) in
    let f = (fun ch -> Netmime_channels.read_mime_message ~multipart_style:`Deep ch) in
    Netchannels.with_in_obj_channel ch f
  (* see http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and the Netstream input bits,
     but it's worth keeping an eye on. *)

  let rec amap f g (tree : parsetree) =
    match tree with
      (header, `Body b) ->
      if f header = header   (* only invoke g (the converting function) if f converts the header *)
      then tree
      else (f header, `Body (b#set_value (g b#value); b))

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
          if f header = header   (* case where we want to modify/ make a copy *)
          then [part]
          else [ (f header, `Body (b#set_value (g b#value); b)); part]
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

  (* seems like it might be nice to have a function w/ the same signature as
     convert for the headers, leaving convert to take care of the actual body
     data  *)
  (* let header_convert (h : #Netmime.mime_header) = *)
  (*   if let (disposition, params_alist) = Netmime_header.(h#get_content_disposition) in *)
  (*     String.lowercase_ascii disposition == "attachment" *)
  (*   then Netmime.(h#field "filename" ^ ".copy" |> h#update_field "filename"); *)
  (*   h *)

  (* NOTE(s): the actual filename is absolutely something we're going to need to mess with, and it's not *)
  (* as easily accessed as other fields -- the params_alist is going to list the parameters like [..., ("filename", p), ...] *)
  (* where p has the type Netmime_string.s_param. *)
  (* Looks like we could get the actual filename as a string by doing something like *)
  (*  mk_param ((param_value p) ^ ".copy")  *)
  (* which is great for just filenames, but it might be worth defining a param_map function that extracts a param val, applies a function to the string, and re-encodes it *)

  (* TODO: should decide how this plays with config to determine e.g. new filename/extension *)
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

