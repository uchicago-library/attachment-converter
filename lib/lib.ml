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
      let h' = header |> header_to_string |> f |> header_from_string in
      if header = h'
      (* only invoke g (the converting function) if f converts the header *)
      then tree
      else (h', `Body (b#set_value (g b#value); b))
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
          let h' = header |> header_to_string |> f |> header_from_string in
          if header = h'
          then [part]
          else [ (h', `Body (b#set_value (g b#value); b)); part]
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




module Practicum = struct

  (* including Owen's demo code from Winter 2021 below *)

  open Mrmime

  let parse_mail =
    Angstrom.(parse_string ~consume:All Mail.mail)

  let parse_mail_file fname =
    readfile fname |> parse_mail

  let unpack_root_header = Result.map fst
  (* match parsed with
   *   Ok(h, _) -> Ok(h)
   * | Error e -> Error e *)

  let unpack_root_mail parsed =
    match parsed with
      Ok(_, m) -> Ok(m)
    | Error e -> Error e


  let from_header (key: Field_name.t) (h: Header.t) =
    if Header.exists key h then
      Ok (Header.assoc key h) else
      Error "Missing field"

  (*  Given a list of optional values, build a list of the contents of any the
   *  non-None options (and ignore Nones) *)
  let take_any opts =
    let rec take_any' opts acc =
      match opts with
      | [] -> acc
      | opt :: rest -> match opt with
        | Some x -> take_any' rest (x :: acc)
        | None -> take_any' rest acc
    in List.rev (take_any' opts [])

  (* DEPRECATED / for convenience writing tests only.
   * Instead of directly accessing leaves, invoke [attach_copy_map] on the Mail tree
   * with a function that wants to be called on an attachment-bearing leaf (and returns one)
   *
   * Given a parsed email (i.e. Mrmime.Mail.t), build a list of the bottom-level
   * leaves, which contain the actual email and attachment content *)
  let rec leaf_list m =
    Mail.(
      match m with
        Leaf _ -> [m]
      | Multipart {body; _} -> List.flatmap leaf_list (take_any body)
      | Message {body; _} ->  leaf_list body
    )


  let unpack_content_type =
    Mail.(
      function
        Leaf {header; _} | Multipart {header;_} | Message {header;_} -> Header.content_type header
    )


  let unpack_content_encoding =
    Mail.(
      function
        Leaf {header; _} | Multipart {header;_} | Message {header;_} -> Header.content_encoding header
    )

  let is_attachment m =
    Content_type.(
      let primary_type = ty (unpack_content_type m) in
      Type.(is_discrete primary_type && not (equal text primary_type))
    )

  (* f should be a function from an attachment-bearing leaf to a better attachment-bearing leaf.
   * See [decode_leaf_body] and [encode_leaf_body] examples.
   *
   * Not a "true" map, since we keep the leaf pre-images with us in the resulting Mail.t.
   * *)
  let rec attach_copy_map f m =
    let rec multipart_body_map = function
        [] -> []
      |  x :: xs -> match x with
          Some leaf ->
          if is_attachment leaf
          then x :: (Some (f leaf) :: multipart_body_map xs) (* Note the "duplicate" leaf *)
          else x :: multipart_body_map xs
        | _ -> multipart_body_map xs
    in
    Mail.(
      match m with
      | Multipart {header; body}
        -> Multipart {header = header; body = multipart_body_map body}
      | Message {header; body}
        -> Message {header = header; body = attach_copy_map f body}
      | Leaf _ -> assert false (* never descend to leaf level -- uses helper.
                                * won't break because even for minimal "hello world" emails,
                                * MrMime parses as multipart w/ a single leaf. - O *)
    )


  (* *)
  let decode_leaf_body = Mail.(function
      | Leaf {header;  body} ->
        let decoded = String.split body |> String.concat "" |> Base64.decode_exn in
        Leaf {header = header;
              body = decoded }
      | _ -> assert false
    )

  (* TODO: insert '\r\n' as appropriate, just in case MrMime needs that. *)
  let reencode_leaf_body  = Mail.(function
      | Leaf {header;  body} ->
        let reencoded = Base64.encode_exn body in
        Leaf {header = header;
              body = reencoded }
      | _ -> assert false
    )

  (* Note: it is beyond me to extract the filename of an attachment from its
   *  parameters, since content-disposition is "unstructured" to MrMime.
   *  This is probably a suitable band-aid.
   *
   * maybe should also add a random number to the string so it's not all a bunch of 'application.pdf'
   * writing over each other *)
  let leaf_to_file = Mail.(function
      | Leaf {header;  body} ->
        let ctype = Header.content_type header in
        let fname = Content_type.(
            (ty ctype |> Type.to_string)
            ^ "."
            ^ (subty ctype |> function `Ietf_token token | `Iana_token token | `X_token token -> token))
        in writefile ~fn:fname body;
        Leaf {header;  body} (* return leaf, unchanged, so this composes a little more easily *)
      | _ -> assert false
    )


  (* ****************************************************************************** *)
  let example () = parse_mail_file "lib/3164_crlf"
  let message_example () = parse_mail_file "digest"

  let leaftest () = Result.((unpack_root_mail @@ example ()) >>= (fun m -> Ok(leaf_list m)))
  let subject () = Result.((unpack_root_header @@ example ()) >>= from_header Field_name.subject)

  let cont_type () = Result.(unpack_root_header (example ()) >>= (fun h -> Ok(Header.content_type h)))

  let print_ctypes_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map (fun m -> unpack_content_type m |> Content_type.ty |> Content_type.Type.to_string ) ls)))
  let print_encoding_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map (fun m -> unpack_content_encoding m ) ls)))
  let is_attachment_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map is_attachment ls)))

  let b64_test () = Result.(
      unpack_root_mail (example ())
      >>= (fun m ->
          Ok(attach_copy_map (fun leaf -> decode_leaf_body leaf |> leaf_to_file |> reencode_leaf_body) m)))

  (* let difftest () =
   *   readfile "application.pdf" |>
   *   Base64.encode_exn |>
   *   writefile ~fn:"b64test.txt"
   *
   * let doeswork () = (readfile "b64test_alt.txt") ^ "\n" |> Base64.decode_exn |> writefile ~fn:"clone" *)
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

