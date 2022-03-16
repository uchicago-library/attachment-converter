
let message err =
  match err with
  | `DummyError     -> "Dummy error message"
  | `ReferParse msg -> msg
  | `ConfigData msg -> msg
