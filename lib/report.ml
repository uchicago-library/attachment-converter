open Prelude

(* note: should work on both mbox and individual email *)
let attachment_types ?(params = false) channel =
  let open String                                                             in
  let module M     = Map.Make(String)                                         in
  let attach_header = "content-type:"                                         in
  let split_index   = len attach_header                                       in
  let attach_cont   = prefix attach_header << lowercase_ascii                 in
  let mime_type     = drop split_index >> trim whitespace >> lowercase_ascii  in
  let just_mime     = takewhile (not << (contains (whitespace ^ ";")))        in
  let is_attach     = not << disjunction [ prefix "multipart" ;
                                           prefix "message"   ;
                                         ]                                    in

  let inc_or_zero k =
    M.update k (fun o -> Some (match o with | None -> 1 | Some x -> (x + 1))) in
  let add_to_set curr line =
    let mime = if   params
               then mime_type line
               else just_mime (mime_type line)
    in
    if   attach_cont line && is_attach mime
    then inc_or_zero mime curr
    else curr                                                                 in
  foldlines add_to_set M.empty channel
