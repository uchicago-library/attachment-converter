open Prelude

module type MBOX = sig
  exception Invalid_mbox

  type t

  type email =
    { from_line : string;
      from_line_num : int;
      after_from_line : string
    }

  val of_in_channel : in_channel -> (t, Error.t) result
  val of_in_channel_exn : in_channel -> t
  val input_email : t -> email option
  val close : t -> unit
  val foldl : (email -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (email -> unit) -> t -> unit
end

module Mbox : MBOX = struct
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

  let of_in_channel_exn (chan : in_channel) : t =
    Option.to_exn Invalid_mbox (of_in_channel_opt chan)

  let close (mbox : t) : unit = In_channel.close mbox.chan

  let input_email (mbox : t) : email option =
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

  let foldl (step : email -> 'a -> 'a) (mbox : t) (base : 'a)
      : 'a =
    let rec go acc =
      match input_email mbox with
      | None -> acc
      | Some email -> go (step email acc)
    in
    go base

  let iter (step : email -> unit) (mbox : t) =
    foldl (fun email _ -> step email) mbox ()
end

module type Parser = sig
  (* an internal channel *)
  type mchan

  (* create an internal channel *)
  val create_mchan : in_channel -> mchan

  val fold :
    ('a -> (string, string) result -> 'a) ->
    in_channel ->
    'a ->
    'a

  (* read a single entity *)
  val read : mchan -> bool -> (string, string) result

  (* convert messages in channel by applying f*)
  val convert :
    in_channel ->
    (string -> (string, 'err) result) ->
    (unit, 'err) result
end

(* Creates an mbox parser *)
module MBoxParser : Parser = struct
  (* a channel wrapper for an mbox *)
  type mchan =
    { (* channel that contains the data *)
      chan : in_channel;
      (* buffer to hold the contents of the channel *)
      buffer : Buffer.t;
      (* next line in the channel *)
      mutable next : string option;
      (* current line number *)
      mutable line_num : int
    }

  (* create an mbox channel with a buffer, an empty peek
     option, and initial line number *)
  let create_mchan chan =
    { chan;
      buffer = Buffer.create 1000;
      next = None;
      line_num = 0
    }

  (* return the current line number *)
  let current_line_number m = m.line_num

  (* get the contents of the buffer *)
  let get_contents m =
    let contents = Buffer.contents m.buffer in
    Buffer.clear m.buffer ;
    Ok contents

  (* look and store the next line *)
  let peek_line m =
    match m.next with
    | Some line -> (Some line, m)
    | None ->
      let result =
        try
          let line = input_line m.chan in
          m.next <- Some line ;
          m.line_num <- m.line_num + 1 ;
          Some line
        with End_of_file -> None
      in
      (result, m)

  let read_line m =
    let record line =
      Buffer.add_string m.buffer line ;
      Buffer.add_char m.buffer '\n'
    in
    match m.next with
    | Some line ->
      m.next <- None ;
      record line
    | None ->
      let line = input_line m.chan in
      m.line_num <- m.line_num + 1 ;
      record line

  let is_from_line line =
    String.length line >= 5 && String.sub line 0 5 = "From "

  let rec read mbox start_of_email =
    match peek_line mbox with
    | None, _ ->
      if Buffer.length mbox.buffer > 0
      then get_contents mbox
      else Error "End of file"
    | Some line, m ->
      if start_of_email && not (is_from_line line)
      then
        Error
          (Printf.sprintf
             "Malformed start of email at line %d: %s"
             (current_line_number m)
             line )
      else if is_from_line line && not start_of_email
      then get_contents m
      else (
        read_line m ;
        read m false )

  let fold fn chan acc =
    let mbox = create_mchan chan in
    let rec loop acc =
      match read mbox true with
      | Ok msg -> loop (fn acc (Ok msg))
      | Error "End of file" -> acc
      | Error err -> loop (fn acc (Error err))
    in
    loop acc

  let convert chan (f : string -> (string, 'err) result) :
      (unit, 'err) result =
    fold
      (fun acc msg ->
        match acc with
        | Error e -> Error e
        | Ok () -> (
          match msg with
          | Error parse_err ->
            prerr_endline ("Parse error: " ^ parse_err) ;
            Ok ()
          | Ok s -> (
            match f s with
            | Ok transformed ->
              output_string stdout transformed ;
              flush stdout ;
              Ok ()
            | Error err -> Error err ) ) )
      chan (Ok ())
end

let () =
  let input_file =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "-"
  in
  let ic =
    if input_file = "-" then stdin else open_in input_file
  in
  let result =
    MBoxParser.convert ic (fun msg ->
        Ok
          ( "--- EMAIL START ---\n" ^ msg
          ^ "--- EMAIL END ---\n\n" ) )
  in
  (* Close input file if it's not stdin *)
  if input_file <> "-" then close_in ic ;
  match result with
  | Ok () -> ()
  | Error err ->
    prerr_endline ("Error: " ^ err) ;
    exit 1
