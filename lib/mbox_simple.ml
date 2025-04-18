open Prelude
(* open Result.Ops *)

module Toy = struct
  let preview str =
    let msg = String.take 50 str in
    printf "Preview:\n%s...\n" msg

  let mbox f chan =
    let open Result.Ops in
    let buf = Buffer.create 1000 in
    let message from chan =
      (* read an email message *)
      let rec loop fromline =
        match readline chan with
        | exception End_of_file -> (None, Buffer.contents buf)
        | line ->
           if String.prefix "From " line
           then (Some line, Buffer.contents buf)
           else begin
               Buffer.add_string buf line ;
               Buffer.add_char buf '\n' ;
               loop fromline
             end
      in
      let () = Buffer.clear buf in
      (* read the first line to see whether it's a From
         line *)
      let* fromline =
        match from with
        | Some line -> Ok line
        | None -> (
          match readline chan with
          | exception End_of_file -> Error "missing from line"
          | line ->
             if not @@ String.prefix "From " line
             then Error ("malformed From line: %s" % line)
             else Ok line )
      in
      Ok (loop fromline)
    in
    let rec loop fromline =
      match message fromline chan with
      | Error e -> prerr_endline e
      | Ok (None, msg) -> f msg
      | Ok (fromline, msg) ->
         f msg ;
         loop fromline
    in
    loop None
end

module MBoxChannel = struct
  (* an mbox channel has a channel that it reads through and a buffer where it stores content*)
  type mchan = {
      (* input channel, or a channel *)
      chan: in_channel;
      (* buffer that stores the information in the channel *)
      buffer: Buffer.t;
      (* the line we've peeked *)
      mutable peeked: string option
    }
  (* internal mbox channe that contains the channel, the buffer, and the peeked data *)
  let create chan = {chan; buffer = Buffer.create 1000; peeked = None}
  (* return the contents of the buffer *)
  let get_contents m =
    let data = Buffer.contents m.buffer in
    Buffer.clear m.buffer; Ok data
  (* look at the next line without "consuming" it *)
  let peek_line m =
    match m.peeked with
    | Some line -> Some line, m
    | None -> match input_line m.chan with
              | line -> m.peeked <- Some line; Some line, m
              | exception End_of_file -> None, m
  let read_line m = 
    let read line = Buffer.add_string m.buffer line ; Buffer.add_char m.buffer '\n' ; in 
    match m.peeked with
    | Some line ->  m.peeked <- None; read line
    | None -> 
       match input_line m.chan with 
       | line -> read line ;
       | exception End_of_file -> raise End_of_file
end
(* given an input channel reads in an email 
   this will also perform validation. The first line
   for whether the channel starts should always be a valid from line
 *)

let read_email input_chan start_of_email =
  (* check whether a line is a from line *)  
  let is_from_line line = String.length line >= 5 && String.sub line 0 5 = "From " in
  (* consume the line and then move on to the next line making sure that it isi not marked as the start *)
  let rec read_line input_chan start_of_email =
    (* get the line from the input channel *)
    match MBoxChannel.peek_line input_chan with (* this will be a peek *)
    (* if you reached the end of the file, return everything you've read so far *)  
    | None, m -> MBoxChannel.get_contents m 
    | Some line, m -> 
       if start_of_email && not(is_from_line line) then Error (Printf.sprintf "malformed start of email. From line: %s" line)
                                                              (* we have successfully read an email since we've reached another from line *)
       else if is_from_line line && not(start_of_email) then MBoxChannel.get_contents input_chan
                                                                                      (* consume the line and continue *)
       else (MBoxChannel.read_line m; read_line m false)
  in read_line input_chan start_of_email

let read_all_mbox chan f =
  let rec _read_all_mbox _chan _f =
    let mbox_channel = MBoxChannel.create chan in 
    match read_email mbox_channel true with
    | Error _ -> []
    | Ok email -> f email ; _read_all_mbox _chan _f
  in _read_all_mbox chan f

let mbox_file_fold fn inchan acc =		(* KW *)
  let ic = MBoxChannel.create inchan in
  let rec loop acc =
    match read_email ic with
    | exception End_of_file -> acc
    | msg -> loop (fn acc msg)
  in loop acc
