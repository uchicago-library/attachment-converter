open Prelude

let attachment_types ?(params = false) =
  let open Strings                                              in
  let attach_header = "Content-Type:"                           in
  let split_index   = len attach_header                         in
  let attach_cont   = prefix attach_header                      in
  let mime_type     = drop split_index >> trim " "              in
  let just_mime     = takewhile (not << (String.contains " ;")) in
  let not_attach = not << disjunction
    [ prefix "multipart" ;
      prefix "message"   ;
    ]
  in
  let open Lists in
  split ~sep:"\n"                      >>
  filter attach_cont                   >>
  map mime_type                        >>
  if params then id else map just_mime >>
  nub                                  >>
  filter not_attach
