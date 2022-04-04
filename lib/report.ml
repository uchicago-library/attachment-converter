open Prelude

(* note: should work on both mbox and individual email *)
let attachment_types ?(params = false) channel =
  let open String                                                      in
  let module SS     = Set.Make(String)                                 in
  let attach_header = "Content-Type:"                                  in
  let split_index   = len attach_header                                in
  let attach_cont   = prefix attach_header                             in
  let downcase      = String.map Char.lowercase_ascii                  in
  let mime_type     = drop split_index >> trim whitespace >> downcase  in
  let just_mime     = takewhile (not << (contains (whitespace ^ ";"))) in
  let is_attach     = not << disjunction [ prefix "multipart" ;
                                           prefix "message"   ;
                                         ]                             in

  let add_to_set curr line =
    let mime = if   params
               then mime_type line
               else just_mime (mime_type line)
    in
    if   attach_cont line && is_attach mime
    then SS.add mime curr
    else curr
  in
  foldlines add_to_set SS.empty channel
