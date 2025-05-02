open Prelude

let preview str =
  let msg = String.take 50 str in
  printf "Preview:\n%s...\n" msg

(* Define a channel type *)
module type Channel = sig
  type t
  val input_line : t -> string
end

module type MBoxChannel = sig
  type mchan
  type source
  val peek_line : mchan -> string option * mchan
  val read_line : mchan -> unit
  val get_contents : mchan -> (string, string) result
  val create : source -> mchan
  val current_line_number : mchan -> int
end

module MBoxChannel (C : Channel) : MBoxChannel with type source = C.t = struct
  type source = C.t

  type mchan = {
    chan: source;
    buffer: Buffer.t;
    mutable peeked: string option;
    mutable line_num: int;
  }

  let create chan = {chan; buffer = Buffer.create 1000; peeked = None; line_num = 0}

  let get_contents m =
    let data = Buffer.contents m.buffer in
    Buffer.clear m.buffer;
    Ok data

  let peek_line m =
    match m.peeked with
    | Some line -> Some line, m
    | None ->
      (match C.input_line m.chan with
       | line ->
         m.peeked <- Some line;
         m.line_num <- m.line_num + 1;
         Some line, m
       | exception End_of_file -> None, m)

  let read_line m =
    let read line =
      Buffer.add_string m.buffer line;
      Buffer.add_char m.buffer '\n'
    in
    match m.peeked with
    | Some line ->
      m.peeked <- None;
      read line
    | None ->
      (match C.input_line m.chan with
       | line ->
         m.line_num <- m.line_num + 1;
         read line
       | exception End_of_file -> raise End_of_file)

  let current_line_number m = m.line_num
end

module MboxReader (MBoxChannel : MBoxChannel) = struct
  let rec read_email (input_chan : MBoxChannel.mchan) start_of_email =
    let is_from_line line =
      String.length line >= 5 && String.sub line 0 5 = "From "
    in
    let rec read_line input_chan start_of_email =
      match MBoxChannel.peek_line input_chan with
      | None, m -> MBoxChannel.get_contents m
      | Some line, m ->
        if start_of_email && not (is_from_line line) then
          Error (Printf.sprintf "Malformed start of email at line %d: %s"
                   (MBoxChannel.current_line_number m) line)
        else if is_from_line line && not start_of_email then
          MBoxChannel.get_contents input_chan
        else (
          MBoxChannel.read_line m;
          read_line m false
        )
    in
    read_line input_chan start_of_email

  let read_all_mbox chan f =
    let rec _read_all_mbox _chan _f =
      let mbox_channel = MBoxChannel.create chan in
      match read_email mbox_channel true with
      | Error _ -> []
      | Ok email ->
        f email;
        _read_all_mbox _chan _f
    in
    _read_all_mbox chan f

  let mbox_file_fold fn inchan acc =
    let ic = MBoxChannel.create inchan in
    let rec loop acc =
      match read_email ic true with
      | exception End_of_file -> acc
      | msg -> loop (fn acc msg)
    in
    loop acc
end

module FileChannel : Channel with type t = in_channel =  struct
  type t = in_channel
  let input_line = input_line
end

module FileMBoxChannel = MBoxChannel(FileChannel)
module FileMBoxReader = MboxReader(FileMBoxChannel)

let () =
  let ic = open_in "mailbox.txt" in
  let mbox_channel = FileMBoxChannel.create ic in
  match FileMBoxReader.read_email mbox_channel true with
  | Ok email -> preview ("Email:\n" ^ email)
  | Error err -> preview ("Error: " ^ err)
