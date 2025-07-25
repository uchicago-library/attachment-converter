open Prelude

exception Invalid_mbox

type t =
  { mutable from_line : string option;
    mutable from_line_num : int;
    chan : in_channel;
    _buf : Buffer.t
  }

type email =
  { from_line : string;
    from_line_num : int;
    after_from_line : string
  }

let is_from_line line =
  String.length line >= 5 && String.sub line 0 5 = "From "

let guard b = if b then Some () else None
let ( let* ) = Option.bind

let of_in_channel_opt (chan : in_channel) : t option =
  let* line = In_channel.input_line chan in
  let* _ = guard (is_from_line line) in
  Some
    { from_line = Some line;
      from_line_num = 1;
      chan;
      _buf = Buffer.create 1000
    }

let of_in_channel chan =
  let module Trace = Error.T in
  Trace.of_option `InvalidMBox (of_in_channel_opt chan)

let of_in_channel_exn chan =
  Option.to_exn Invalid_mbox (of_in_channel_opt chan)

let close mbox = In_channel.close mbox.chan

let input_email mbox =
  let _ = Buffer.clear mbox._buf in
  let rec go count =
    let* from_line = mbox.from_line in
    match In_channel.input_line mbox.chan with
    | None -> begin
        mbox.from_line <- None ;
        Some
          { from_line;
            from_line_num = mbox.from_line_num;
            after_from_line = Buffer.contents mbox._buf
          }
      end
    | Some line ->
      if is_from_line line
      then
        let from_line_num = mbox.from_line_num in
        begin
          mbox.from_line <- Some line ;
          mbox.from_line_num <- mbox.from_line_num + count ;
          Some
            { from_line;
              from_line_num;
              after_from_line = Buffer.contents mbox._buf
            }
        end
      else begin
        Buffer.add_string mbox._buf line ;
        Buffer.add_char mbox._buf '\n' ;
        go (count + 1)
      end
  in
  go 1

let foldl step mbox base =
  let rec go acc =
    match input_email mbox with
    | None -> acc
    | Some email -> go (step email acc)
  in
  go base

let iter step mbox =
  foldl (fun email _ -> step email) mbox ()
