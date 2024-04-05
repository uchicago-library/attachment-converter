type line_feed = Dos | Unix | Apple

let check_file filepath =
  let open Prelude in
  let dipstick () = within readline filepath in
  match ,
  | exception _ -> Apple
  | _ -> 
  
  match dipstick.[String.length dipstick - 1] with
  | exception _ -> Apple
  | 'r' -> Dos
  | _ -> Unix

