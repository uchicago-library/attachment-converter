open Prelude
(* open Result.Ops *)

let preview str =
  let msg = String.take 50 str in
  printf "Preview: %s..." msg

let mbox chan =
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
  let rec loop fromline acc =
    match message fromline chan with
    | Error e ->
      prerr_endline e ;
      rev acc
    | Ok (None, msg) ->
      preview msg ;
      rev ((fromline, msg) :: acc)
    | Ok (fromline, msg) ->
      preview msg ;
      loop fromline ((fromline, msg) :: acc)
  in
  loop None [] |> ignore
