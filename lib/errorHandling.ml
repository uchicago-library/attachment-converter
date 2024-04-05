open Utils
module _ : ERROR = Configuration.Formats.Error
module _ : ERROR = Header.Field.Value.Error
module _ : ERROR = Convert.Converter.Error
module _ : ERROR = Dependency.Error

module Error :
  ERROR
    with type t =
      [ Convert.Converter.Error.t
      | Configuration.Formats.Error.t
      | Dependency.Error.t ] = struct
  type t =
    [ Convert.Converter.Error.t
    | Configuration.Formats.Error.t
    | Dependency.Error.t ]

  let message err =
    match err with
    | #Convert.Converter.Error.t as e ->
      Convert.Converter.Error.message e
    | #Configuration.Formats.Error.t as e ->
      Configuration.Formats.Error.message e
    | #Dependency.Error.t as e -> Dependency.Error.message e
end

module Printer = struct
  open Prelude

  let print msg = write stderr msg
end
