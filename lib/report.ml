open Prelude

(* note: should work on both mbox and individual email *)
let content_types ?(params = false) channel =
  let open String in
  let module M = Map.Make (String) in
  let content_header = "content-type:" in
  let split_index = len content_header in
  let is_ct_line =
    prefix content_header << lowercase_ascii
  in
  let mime_type =
    drop split_index >> trim whitespace >> lowercase_ascii
  in
  let just_mime =
    takewhile (not << contains (whitespace ^ ";"))
  in
  let inc_or_zero k =
    M.update k (fun o ->
        Some
          ( match o with
          | None -> 1
          | Some x -> x + 1 ) )
  in
  let add_to_set curr line =
    let mime =
      if params
      then mime_type line
      else just_mime (mime_type line)
    in
    if is_ct_line line then inc_or_zero mime curr else curr
  in
  foldlines add_to_set M.empty channel
