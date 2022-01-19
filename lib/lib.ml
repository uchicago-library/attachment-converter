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
  val convert : filepath list -> (string -> string)
  val acopy_email : string -> (string -> string) -> string
end

module Conversion_ocamlnet (* : CONVERT *) = struct

  type htransform = string -> string
  type btransform = string -> string
  type filepath =  string
  type parsetree = Netmime.complex_mime_message


  let parse (s : string) =
    let ch = (new Netstream.input_stream (new Netchannels.input_string s)) in
    let f = (fun ch -> Netmime_channels.read_mime_message ~multipart_style:`Deep ch) in
    Netchannels.with_in_obj_channel ch f
  (* see http://projects.camlcity.org/projects/dl/ocamlnet-4.1.9/doc/html-main/Netmime_tut.html
     -- I /think/ that with_in_obj_channel should close both the Netchannels and the Netstream input bits,
     but it's worth keeping an eye on. *)


  let field_params_alist header fieldname =
    match tail (String.cuts ~sep:";" (header#field fieldname)) with
      Some [] -> [(fieldname, (header#field fieldname))] (* Case 1: no params, alist is just field name/value *)
    | Some params -> let kvpairs = map (String.cut ~sep:"=") params in (* case 2: split on "=" then zip together as alist*)
      map (function (key, Some v) -> (key, v) | (key, None) -> (key, "")) kvpairs
    | None -> assert false

  (*let get_parts = function `Parts plst -> plst | _-> assert false;;*)


  let header_to_string (h : Netmime.mime_header) =
    let buf = Stdlib.Buffer.create 1024 in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      (fun ch -> Netmime_string.write_header ch (h#fields));
    Stdlib.Buffer.contents buf

  let header_from_string s =
    new Netmime.basic_mime_header
      (Netchannels.with_in_obj_channel
         (new Netchannels.input_string s)
         (fun ch -> Netmime_string.read_header ?downcase:(Some false) (new Netstream.input_stream ch)))

  (* param_map takes a builds a string -> string function from a specified header field (e.g. "Content-Disposition"),
   * a parameter that we might find in that field (e.g. "filename"), and a
   * simpler string->string function that just targets that value (e.g. a
   * change_extension function or something.)
  *)
  (* let param_map = () *)


  (* let mk_header  = assert false *)

  (* Notes:
     Content-Disposition headers provide information about how to present a
     message or a body part. When a body part is to be treated as an attached
     file, the Content-Disposition header will include a file name parameter. *)


  let rec amap f g (tree : parsetree) =
    match tree with
      (header, `Body b) ->
      let h = header_to_string header in
      if f h = h
      (* only invoke g (the converting function) if f converts the header *)
      then tree
      else (header_from_string (f h), `Body (b#set_value (g b#value); b))
    | (header, `Parts p_lst) ->
      (header, `Parts (List.map (amap f g) p_lst))

  let rec header_alists (t: parsetree) =
    match t with
      (h, `Body _) ->
      let parse_rest (fname, vals) =
        (fname, Netmime_string.scan_value_with_parameters_ep vals [])
      in map parse_rest h#fields
    | (_, `Parts p_lst) -> List.concat_map header_alists p_lst

  let rec acopy f g (tree : parsetree) =
    match tree with
      (_, `Body _) -> tree (* TODO double check desired behavior for root messages without attachments *)

    | (header, `Parts p_lst) ->
      let copy_or_skip part = (* NOTE: two of the three cases here are singleton
                                 lists, which might be a code smell.  Worth
                                 reviewing in case there's a cleaner way to
                                 express this, especially since it's always
                                 *exactly* one or two things *)
        match part with
          (header, `Body (b: Netmime.mime_body)) ->
          let h = header_to_string header in
          if f h = h
          then [part]
          else [ (header_from_string (f h), `Body (b#set_value (g b#value); b)); part]
        | _ -> [acopy f g part]

      in (header, `Parts List.(concat_map copy_or_skip p_lst))

  let to_string (tree : parsetree) =
    let (header, _) = tree in
    let n = try Netmime_header.get_content_length header
      with Not_found -> (1024 * 1024) in (* defaulting to a megabyte seems like a nice round number, if it isn't overkill *)
    let buf =  Stdlib.Buffer.create n in
    Netchannels.with_out_obj_channel
      (new Netchannels.output_buffer buf)
      (fun ch -> Netmime_channels.write_mime_message ch tree);
    Stdlib.Buffer.contents buf

  (* returns a function that expects and returns binary attachment data as a string, not b64 encoded.
     e.g. `body # set_value (convert ["/bin/cat" "-"] (body # value))` would be a noop, since the ocamlnet value access
     methods handle the encoding themselves. *)
  let convert =
    Prelude.Unix.Proc.rw ?oknon0:(Some false)  ?env:None ?usepath:(Some true)

  let is_attachment (tree : parsetree) =
    let (header, _) = tree in
    let s = try header # field "content-disposition"
      with Not_found -> ""
    in Strings.prefix "attachment" (String.lowercase_ascii s)

  let unparts pts =
    match pts with
      `Parts plist -> plist
    | _ -> assert false

  let unbody bdy =
    match bdy with
      `Body b -> b
    | _ -> assert false

  let xmas_tree () =
    let (_, parts) = parse (readfile "../2843") in
    match unparts parts with
      (_ :: attached :: _ ) -> attached
    | _ -> assert false

  let update_filename hstr ?(ext="") =
    let open Strings in
    let open Filename in
    match substr "filename=" hstr with
      Some lo ->
      let hi = lo + try Option.get (substr "\r\n" hstr#.(lo,0)) with
          Invalid_argument _ -> assert false (* should never happen if libpst does its job *)
      in
      let old_name =    (* offsets are for escaped quotation marks and/or a ';',
                           which is only required if more params follow the filename param.*)
        if mem ';' hstr#.(lo, hi)
        then hstr#.(lo + 10, hi - 2)
        else hstr#.(lo + 10, hi - 1)
      in
      let new_name = (remove_extension old_name)
                     ^ ".CONVERTED.yyyymmdd"
                     ^ (extension old_name)
                     ^ ext
      in replace old_name new_name hstr
    | _ -> assert false

  (* TESTS *)
  let pdf_convert_test fname =
    let header_func hstring =
      let hs = String.lowercase_ascii hstring in
      match Strings.(
          (substr "content-disposition: attachment;" hs,
           substr "content-type: application/pdf" hs)
        )
      with
        (Some _, Some _) ->
        update_filename hstring ~ext:""
      | _ -> hstring (* noop when not a pdf and attachment *)
    in
    let body_func _bstring =
      "[REDACTED]" (*todo call convert with some utility*)
    in
    let tree = parse (readfile fname) in
    let converted_tree = amap header_func body_func tree
    in writefile ~fn:(fname ^ "_xmas_cancelled") (to_string converted_tree)


  let upcase_header_and_delete_body fname =
    let f = String.uppercase_ascii in
    let g = fun _ -> "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_upcased_and_deleted") (tree |> (amap f g) |> to_string)

  let omit_gore_y_details fname =
    let f = Fun.const "Content-Type: application/json\r\n\r\n" in
    (* let f = fun _ -> "From: Bill Clinton <president@whitehouse.gov>\r\n\r\n" in *)
    let g = Fun.const "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_contented") (tree |> (amap f g) |> to_string)

  let acopy_email () = assert false



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


module Practicum = struct

  (* (\* including Owen's demo code from Winter 2021 below *\) *)

  (* open Mrmime *)

  (* let parse_mail = *)
  (*   Angstrom.(parse_string ~consume:All Mail.mail) *)

  (* let parse_mail_file fname = *)
  (*   readfile fname |> parse_mail *)

  (* let unpack_root_header = Result.map fst *)
  (* (\* match parsed with *)
  (*  *   Ok(h, _) -> Ok(h) *)
  (*  * | Error e -> Error e *\) *)

  (* let unpack_root_mail parsed = *)
  (*   match parsed with *)
  (*     Ok(_, m) -> Ok(m) *)
  (*   | Error e -> Error e *)


  (* let from_header (key: Field_name.t) (h: Header.t) = *)
  (*   if Header.exists key h then *)
  (*     Ok (Header.assoc key h) else *)
  (*     Error "Missing field" *)

  (* (\*  Given a list of optional values, build a list of the contents of any the *)
  (*  *  non-None options (and ignore Nones) *\) *)
  (* let take_any opts = *)
  (*   let rec take_any' opts acc = *)
  (*     match opts with *)
  (*     | [] -> acc *)
  (*     | opt :: rest -> match opt with *)
  (*       | Some x -> take_any' rest (x :: acc) *)
  (*       | None -> take_any' rest acc *)
  (*   in List.rev (take_any' opts []) *)

  (* (\* DEPRECATED / for convenience writing tests only. *)
  (*  * Instead of directly accessing leaves, invoke [attach_copy_map] on the Mail tree *)
  (*  * with a function that wants to be called on an attachment-bearing leaf (and returns one) *)
  (*  * *)
  (*  * Given a parsed email (i.e. Mrmime.Mail.t), build a list of the bottom-level *)
  (*  * leaves, which contain the actual email and attachment content *\) *)
  (* let rec leaf_list m = *)
  (*   Mail.( *)
  (*     match m with *)
  (*       Leaf _ -> [m] *)
  (*     | Multipart {body; _} -> List.flatmap leaf_list (take_any body) *)
  (*     | Message {body; _} ->  leaf_list body *)
  (*   ) *)


  (* let unpack_content_type = *)
  (*   Mail.( *)
  (*     function *)
  (*       Leaf {header; _} | Multipart {header;_} | Message {header;_} -> Header.content_type header *)
  (*   ) *)


  (* let unpack_content_encoding = *)
  (*   Mail.( *)
  (*     function *)
  (*       Leaf {header; _} | Multipart {header;_} | Message {header;_} -> Header.content_encoding header *)
  (*   ) *)

  (* let is_attachment m = *)
  (*   Content_type.( *)
  (*     let primary_type = ty (unpack_content_type m) in *)
  (*     Type.(is_discrete primary_type && not (equal text primary_type)) *)
  (*   ) *)

  (* (\* f should be a function from an attachment-bearing leaf to a better attachment-bearing leaf. *)
  (*  * See [decode_leaf_body] and [encode_leaf_body] examples. *)
  (*  * *)
  (*  * Not a "true" map, since we keep the leaf pre-images with us in the resulting Mail.t. *)
  (*  * *\) *)
  (* let rec attach_copy_map f m = *)
  (*   let rec multipart_body_map = function *)
  (*       [] -> [] *)
  (*     |  x :: xs -> match x with *)
  (*         Some leaf -> *)
  (*         if is_attachment leaf *)
  (*         then x :: (Some (f leaf) :: multipart_body_map xs) (\* Note the "duplicate" leaf *\) *)
  (*         else x :: multipart_body_map xs *)
  (*       | _ -> multipart_body_map xs *)
  (*   in *)
  (*   Mail.( *)
  (*     match m with *)
  (*     | Multipart {header; body} *)
  (*       -> Multipart {header = header; body = multipart_body_map body} *)
  (*     | Message {header; body} *)
  (*       -> Message {header = header; body = attach_copy_map f body} *)
  (*     | Leaf _ -> assert false (\* never descend to leaf level -- uses helper. *)
  (*                               * won't break because even for minimal "hello world" emails, *)
  (*                               * MrMime parses as multipart w/ a single leaf. - O *\) *)
  (*   ) *)


  (* (\* *\) *)
  (* let decode_leaf_body = Mail.(function *)
  (*     | Leaf {header;  body} -> *)
  (*       let decoded = String.split body |> String.concat "" |> Base64.decode_exn in *)
  (*       Leaf {header = header; *)
  (*             body = decoded } *)
  (*     | _ -> assert false *)
  (*   ) *)

  (* (\* TODO: insert '\r\n' as appropriate, just in case MrMime needs that. *\) *)
  (* let reencode_leaf_body  = Mail.(function *)
  (*     | Leaf {header;  body} -> *)
  (*       let reencoded = Base64.encode_exn body in *)
  (*       Leaf {header = header; *)
  (*             body = reencoded } *)
  (*     | _ -> assert false *)
  (*   ) *)

  (* (\* Note: it is beyond me to extract the filename of an attachment from its *)
  (*  *  parameters, since content-disposition is "unstructured" to MrMime. *)
  (*  *  This is probably a suitable band-aid. *)
  (*  * *)
  (*  * maybe should also add a random number to the string so it's not all a bunch of 'application.pdf' *)
  (*  * writing over each other *\) *)
  (* let leaf_to_file = Mail.(function *)
  (*     | Leaf {header;  body} -> *)
  (*       let ctype = Header.content_type header in *)
  (*       let fname = Content_type.( *)
  (*           (ty ctype |> Type.to_string) *)
  (*           ^ "." *)
  (*           ^ (subty ctype |> function `Ietf_token token | `Iana_token token | `X_token token -> token)) *)
  (*       in writefile ~fn:fname body; *)
  (*       Leaf {header;  body} (\* return leaf, unchanged, so this composes a little more easily *\) *)
  (*     | _ -> assert false *)
  (*   ) *)


  (* (\* ****************************************************************************** *\) *)
  (* let example () = parse_mail_file "lib/3164_crlf" *)
  (* let message_example () = parse_mail_file "digest" *)

  (* let leaftest () = Result.((unpack_root_mail @@ example ()) >>= (fun m -> Ok(leaf_list m))) *)
  (* let subject () = Result.((unpack_root_header @@ example ()) >>= from_header Field_name.subject) *)

  (* let cont_type () = Result.(unpack_root_header (example ()) >>= (fun h -> Ok(Header.content_type h))) *)

  (* let print_ctypes_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map (fun m -> unpack_content_type m |> Content_type.ty |> Content_type.Type.to_string ) ls))) *)
  (* let print_encoding_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map (fun m -> unpack_content_encoding m ) ls))) *)
  (* let is_attachment_test () = Result.(leaftest () >>= (fun ls -> Ok(List.map is_attachment ls))) *)

  (* let b64_test () = Result.( *)
  (*     unpack_root_mail (example ()) *)
  (*     >>= (fun m -> *)
  (*         Ok(attach_copy_map (fun leaf -> decode_leaf_body leaf |> leaf_to_file |> reencode_leaf_body) m))) *)

  (* (\* let difftest () = *)
  (*  *   readfile "application.pdf" |> *)
  (*  *   Base64.encode_exn |> *)
  (*  *   writefile ~fn:"b64test.txt" *)
  (*  * *)
  (*  * let doeswork () = (readfile "b64test_alt.txt") ^ "\n" |> Base64.decode_exn |> writefile ~fn:"clone" *\) *)
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
