module Formats = struct

  type htransform = string -> string
  type dtransform = string -> string

  type variety =
    | DataOnly of dtransform
    | DataAndHeader of (htransform * dtransform)
    | NoChange

  module Dict = Map.Make (String)

  type t = (variety list) Dict.t

  module Error = struct
    type t =
      | ReferParse of string
      | ConfigData of string
      | Unix of (string * Unix.error)
      | MimeParse of string
      | CharacterEncoding of string
  end
  type error = Error.t

end
