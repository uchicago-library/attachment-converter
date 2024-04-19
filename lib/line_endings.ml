type line_feed = Dos | Unix | Apple

let check_file filepath =
  let open Prelude in
  (* TODO: fix this logic, probably can't use readline *)
  (* use a file handle version of takeWhile (/= '\r') on the file
     handle *)
  let dipstick () = within readline filepath in
  let bang_bang n str = Prelude.String.get str n in
  match
    dipstick () |> String.rev |> bang_bang 0
  with
  (* | exception End_of_file -> Apple *)
  | '\r' -> Dos
  | _ -> Unix


