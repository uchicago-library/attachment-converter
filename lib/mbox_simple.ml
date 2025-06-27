open Prelude

module type Parser = sig
  (* an internal channel *)
  type mchan
  
  (* create an internal channel *)
  val create_mchan : in_channel -> mchan

  val fold : ('a -> (string, string) result -> 'a) -> in_channel -> 'a -> 'a

  (* read a single entity *)
  val read : mchan -> bool -> (string, string) result

  type iterator
  val of_channel : in_channel -> iterator
  val next : iterator -> (string, string) result

end

(*
  Creates an mbox parser
*)
module MBoxParser : Parser = struct
  
  (* a channel wrapper for an mbox *)
  type mchan = {
    (* channel that contains the data *)
    chan: in_channel;

    (* buffer to hold the contents of the channel *)
    buffer: Buffer.t;

    (* next line in the channel *)
    mutable next: string option;

    (* current line number *)
    mutable line_num: int;
  }

  type iterator = {
    mbox: mchan;
    mutable finished: bool;
  }
  (* create an mbox channel with a buffer, an empty peek option, and initial line number *)
  let create_mchan chan = { chan; buffer = Buffer.create 50000; next = None; line_num = 0 }

  (* return the current line number *)
  let current_line_number m = m.line_num

  (* get the contents of the buffer *)
  let get_contents m =
    let contents = Buffer.contents m.buffer in Buffer.clear m.buffer;
    Ok contents

  (* look and store the next line *)
  let peek_line m =
    match m.next with
    | Some line -> Some line, m
    | None ->
      let result =
        try
          let line = input_line m.chan in
          m.next <- Some line;
          m.line_num <- m.line_num + 1;
          Some line
        with End_of_file -> None
      in result, m

  let read_line m =
    let record line =
      Buffer.add_string m.buffer line;
       Buffer.add_string m.buffer (eol LF) 
    in
    match m.next with
    | Some line ->
        m.next <- None;
        record line
    | None ->
        let line = input_line m.chan in
        m.line_num <- m.line_num + 1;
        record line

  let is_from_line line = String.length line >= 5 && String.sub line 0 5 = "From "
  
  let rec read mbox start_of_email =
  match peek_line mbox with
  | None, _ ->
      if Buffer.length mbox.buffer > 0 then
        get_contents mbox
      else
        Error "End of file"
  | Some line, m ->
      if start_of_email then (
        if is_from_line line then (
          (* At the start: skip first From line, don't add to buffer *)
          m.next <- None;
          read m false
        ) else
          Error (Printf.sprintf "Malformed start of email at line %d: %s"
                   (current_line_number m) line)
      )
      else if is_from_line line then
        (* End of email detected *)
        get_contents m
      else (
        read_line m;
        read m false
      )


  let fold fn chan acc =
    let mbox = create_mchan chan in
    let rec loop acc =
      match read mbox true with
      | Ok msg -> loop (fn acc (Ok msg))
      | Error "End of file" -> acc
      | Error err -> loop (fn acc (Error err))
    in
    loop acc
    
let of_channel chan =
  { mbox = create_mchan chan; finished = false }

let next it =
  if it.finished then Error "End of file"
  else
    match read it.mbox true with
    | Ok msg -> Ok msg
    | Error "End of file" ->
        it.finished <- true;
        Error "End of file"
    | Error err -> Error err

end

module ToOutput = struct
  module Make (T : Convert.PARSETREE) = struct
    
   let convert_mbox in_chan converter =
      let iterator = MBoxParser.of_channel in_chan in
      let rec loop () =
        match MBoxParser.next iterator with
        | Ok msg -> (
            output_string stdout (converter msg);
            flush stdout;
            loop ()
          )
        | Error _ -> Ok ()
      in
      loop ()

    let acopy_mbox ?(idem = true) config in_chan pbar = 
      let module C = Convert.Conversion.Make (T) in 
        let converter em =
          match C.acopy_email ~idem config em pbar with
          | Ok converted -> converted
          | Error errlist ->
            Utils.print_err (Error_message.message errlist) ;
            em
          in 
        convert_mbox in_chan converter
  end
end