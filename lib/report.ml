open Prelude

(* note: should work on both mbox and individual email *)

let count_line f p =
  let open String in
  let module M = Map.Make(String) in
  let inc_or_zero key =
    M.update key (fun curr -> Some (match curr with | None -> 1 | Some n -> (n + 1)))
  in
  let add_to_set curr line =
    if p line
    then inc_or_zero (f line) curr
    else curr
  in
  foldlines add_to_set M.empty

  let content_types ?(params = false) =
    let content_header = "content-type:" in
    let split_index = len content_header in
    let f line =
      let mime_type = drop split_index >> trim whitespace >> lowercase_ascii in
      let just_mime = takewhile (not << (contains (whitespace ^ ";"))) in
      if params
      then mime_type line
      else just_mime (mime_type line)
    in
    let p line = prefix content_header << lowercase_ascii in
    count_line f p

(*
  let content_types ?(params = false) channel =
  let open String                                                              in
  let module M       = Map.Make(String)                                        in
  let content_header = "content-type:"                                         in
  let split_index    = len content_header                                      in
  let is_ct_line     = prefix content_header << lowercase_ascii                in
  let mime_type      = drop split_index >> trim whitespace >> lowercase_ascii  in
  let just_mime      = takewhile (not << (contains (whitespace ^ ";")))        in

  let inc_or_zero k =
    M.update k (fun o -> Some (match o with | None -> 1 | Some x -> (x + 1)))  in
  let add_to_set curr line =
    let mime = if   params
               then mime_type line
               else just_mime (mime_type line)
    in
    if   is_ct_line line
    then inc_or_zero mime curr
    else curr                                                                  in
  foldlines add_to_set M.empty channel
*)
