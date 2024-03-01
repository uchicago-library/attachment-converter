(* ------------------------------------------------------------------
   This is an adaptation of mt.ml in the mrmime library
   which is better suited for serialization

   Nathan Mull, 10/12/22
   ------------------------------------------------------------------ *)

open Mrmime
open Prelude

type t = { header : Header.t; body : string Mail.t option }
type stream = Mt.buffer Mt.stream

let make (header, body) : t = { header; body }

(* ALL DEFINITIONS TAKEN VERBATIM : START *)
let iter ~f buf ~off ~len =
  for i = off to len - 1 do
    f buf.[i]
  done

let to_quoted_printable : ?length:int -> stream -> stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Pecu.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in
  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len = chunk_length - Pecu.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> go ()
    | `Partial ->
      let len = chunk_length - Pecu.dst_rem encoder in
      Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
      Pecu.dst encoder chunk 0 chunk_length ;
      match Pecu.encode encoder `Await with
      | `Ok -> (go [@tailcall]) ()
      | `Partial -> (emit [@tailcall]) () )
    (* XXX(dinosaure): [257] was an old state which is not
       used anymore. *)
    | 258 (* End *) ->
      Ke.Rke.cons queue 259 ;
      (pending [@tailcall]) (Pecu.encode encoder `End)
    | 259 ->
      assert (Pecu.encode encoder `Await = `Ok) ;
      Ke.Rke.cons queue 259 ;
      None
    | 260 -> (
      match Pecu.encode encoder `Line_break with
      | `Ok -> (go [@tailcall]) ()
      | `Partial -> (emit [@tailcall]) () )
    | chr -> (
      match Pecu.encode encoder (`Char (Char.chr chr)) with
      | `Ok -> (go [@tailcall]) ()
      | `Partial -> (emit [@tailcall]) () )
    | exception Ke.Rke.Empty -> (
      match stream () with
      | Some (buf, off, len) ->
        iter
          ~f:(fun chr -> Ke.Rke.push queue (Char.code chr))
          buf ~off ~len ;
        Ke.Rke.push queue 260 ;
        (go [@tailcall]) ()
      | None ->
        Ke.Rke.push queue 258 ;
        (go [@tailcall]) () )
  in
  Pecu.dst encoder chunk 0 chunk_length ;
  go

let to_base64 : ?length:int -> stream -> stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Base64_rfc2045.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in
  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len =
      chunk_length - Base64_rfc2045.dst_rem encoder
    in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> (go [@tailcall]) ()
    | `Partial ->
      let len =
        chunk_length - Base64_rfc2045.dst_rem encoder
      in
      Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
      Base64_rfc2045.dst encoder chunk 0 chunk_length ;
      match Base64_rfc2045.encode encoder `Await with
      | `Ok -> (go [@tailcall]) ()
      | `Partial -> (emit [@tailcall]) () )
    | 257 (* End *) ->
      Ke.Rke.cons queue 258 ;
      (pending [@tailcall])
        (Base64_rfc2045.encode encoder `End)
    | 258 ->
      assert (Base64_rfc2045.encode encoder `Await = `Ok) ;
      Ke.Rke.cons queue 258 ;
      None
    | chr -> (
      match
        Base64_rfc2045.encode encoder (`Char (Char.chr chr))
      with
      | `Ok -> (go [@tailcall]) ()
      | `Partial -> (emit [@tailcall]) () )
    | exception Ke.Rke.Empty -> (
      match stream () with
      | Some (buf, off, len) ->
        iter
          ~f:(fun chr -> Ke.Rke.push queue (Char.code chr))
          buf ~off ~len ;
        (go [@tailcall]) ()
      | None ->
        Ke.Rke.push queue 257 ;
        (go [@tailcall]) () )
  in
  Base64_rfc2045.dst encoder chunk 0 chunk_length ;
  go

let concat s0 s1 =
  let c = ref s0 in
  let rec go () =
    match !c () with
    | Some x -> Some x
    | None ->
      if !c == s0
      then (
        c := s1 ;
        go () )
      else None
  in
  go

let stream_of_string str : stream =
  let consumed = ref false in
  fun () ->
    match !consumed with
    | true -> None
    | false ->
      consumed := true ;
      Some (str, 0, String.length str)

let stream_of_lines lines =
  let lines = ref lines in
  let go () =
    match !lines with
    | [] -> None
    | x :: r ->
      lines := r ;
      Some (x ^ "\r\n", 0, String.length x + 2)
  in
  go

let crlf () = stream_of_string "\r\n"
let ( @ ) a b = concat a b

let map f stream =
  let go () =
    match stream () with
    | Some v -> Some (f v)
    | None -> None
  in
  go

(* ALL DEFINITIONS TAKEN VERBATIM : END The following
   definitions are not verbatim, but are close adaptations
   of other functions in mt.ml *)

let stream_of_header header =
  map
    (fun s -> (s, 0, String.length s))
    (Prettym.to_stream Header.Encoder.header header)

let stream_of_header_and_encoded_body header stream =
  let content_encoding = Header.content_encoding header in
  let body_stream =
    match content_encoding with
    | `Quoted_printable -> to_quoted_printable stream
    | `Base64 -> to_base64 stream
    | `Bit8 | `Binary | `Bit7 -> stream
    | `Ietf_token _ | `X_token _ -> assert false
  in
  stream_of_header header @ crlf () @ body_stream

let stream_of_leaf header data =
  stream_of_header_and_encoded_body header
    (stream_of_string data)

let rec stream_of_multipart header parts =
  let boundary =
    match
      Content_type.boundary (Header.content_type header)
    with
    | Some v -> "--" ^ v (* From Rfc2046 *)
    | None -> Fmt.failwith "Multipart MUST have a boundary"
  in
  let beginner = boundary ^ "\r\n" in
  let inner () = stream_of_lines [ ""; boundary ] in
  let closer = stream_of_lines [ ""; boundary ^ "--" ] in
  let rec go stream = function
    | [] -> assert false
    | [ x ] -> stream @ to_stream (make x) @ closer
    | x :: r ->
      let stream = stream @ to_stream (make x) @ inner () in
      go stream r
  in
  stream_of_header_and_encoded_body header
    (go (stream_of_string beginner) parts)

and stream_of_message header msg_header msg_body =
  stream_of_header_and_encoded_body header
    (to_stream (make (msg_header, Some msg_body)))

and stream_of_header_and_body header (body : string Mail.t)
    =
  match body with
  | Leaf data -> stream_of_leaf header data
  | Multipart parts -> stream_of_multipart header parts
  | Message (msg_header, msg_body) ->
    stream_of_message header msg_header msg_body

and to_stream part =
  match part.body with
  | None ->
    stream_of_header part.header (* TODO: Is this right?? *)
  | Some body -> stream_of_header_and_body part.header body

let to_string part =
  let stream = to_stream part in
  let buffer = Buffer.create 0x1000 in
  let rec go () =
    match stream () with
    | Some (str, off, len) ->
      Buffer.add_substring buffer str off len ;
      go ()
    | None -> Buffer.contents buffer
  in
  go ()
