open Prelude

let attachment_types =
  let open Strings                                         in
  let attach_header = "Content-Type:"                      in
  let split_index   = len attach_header                    in
  let attach_cont   = trimleft " " >> prefix attach_header in
  let mime_type     = trim " " >> drop split_index         in
  let not_multi     = not << prefix "multipart"            in
  let open Lists                                           in
  split ~sep:"\n"    >>
  filter attach_cont >>
  map mime_type      >>
  nub                >>
  filter not_multi
