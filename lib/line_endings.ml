type line_feed = Dos | Unix | Apple

let check_file filepath =
  let open Prelude in
  let dipstick () = within readline filepath in
  match (dipstick ()).[String.length (dipstick ()) - 1] with
  | exception End_of_file -> Apple
  | '\r' -> Dos
  | _ -> Unix
