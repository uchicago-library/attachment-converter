open Prelude

let attachment_types =
  let open Strings                                 in
  let attach_header = "Content-Type:"              in
  let split_index   = len attach_header            in
  let attach_cont   = prefix attach_header         in
  let mime_type     = drop split_index >> trim " " in
  let not_multi     = not << prefix "multipart"    in
  let open Lists                                   in
  split ~sep:"\n"    >>
  filter attach_cont >>
  map mime_type      >>
  nub                >>
  filter not_multi
