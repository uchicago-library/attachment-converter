open Prelude

let attachment_types ?(params = false) channel =
  let open String                                               in
  let module SS     = Set.Make(String)                          in
  let attach_header = "Content-Type:"                           in
  let split_index   = len attach_header                         in
  let attach_cont   = prefix attach_header                      in
  let mime_type     = drop split_index >> trim " "              in
  let just_mime     = takewhile (not << (contains " ;"))        in
  let is_attach     = not << disjunction [ prefix "multipart" ;
                                           prefix "message"   ;
                                         ]                      in

  let add_to_set curr line =
    let mime = if   params
               then mime_type line
               else just_mime (mime_type line)
    in
    if   attach_cont line && is_attach mime
    then SS.add mime curr
    else curr
  in
  SS.to_list (foldlines add_to_set SS.empty channel)

(* This is not used by the executable, but may be useful
   in the future *)
let attachment_types_from_str ?(params = false) =
  let open String                                               in
  let attach_header = "Content-Type:"                           in
  let split_index   = len attach_header                         in
  let attach_cont   = prefix attach_header                      in
  let mime_type     = drop split_index >> trim " "              in
  let just_mime     = takewhile (not << (contains " ;"))        in
  let not_attach    = not << disjunction [ prefix "multipart" ;
                                           prefix "message"   ;
                                         ]                      in
  let open Lists                                                in
  split ~sep:"\n"                      >>
  filter attach_cont                   >>
  map mime_type                        >>
  if params then id else map just_mime >>
  nub                                  >>
  filter not_attach
