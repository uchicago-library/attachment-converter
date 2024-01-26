module Type = struct
  type t =
    | Text
    | Image
    | Audio
    | Video
    | Application
    | Message
    | Multipart
    | Other of string

  let to_string ty =
    match ty with
    | Text -> "text"
    | Image -> "image"
    | Audio -> "audio"
    | Video -> "video"
    | Application -> "application"
    | Message -> "message"
    | Multipart -> "multipart"
    | Other name -> name

  let of_string str =
    match String.lowercase_ascii str with
    | "text" -> Text
    | "image" -> Image
    | "audio" -> Audio
    | "video" -> Video
    | "application" -> Application
    | "message" -> Message
    | "multipart" -> Multipart
    | other -> Other other
end

module Subtype = struct
  type t = { name : string }

  let to_string st = st.name
  let of_string name = { name = name }
end

module Error = struct
  type t = [
    | `MimeType
    ]

  let message err =
    match err with
    | `MimeType -> "Cannot parse mime type"
end

type t =
  { typ : Type.t;
    subtype : Subtype.t;
  }

let type_of mt = mt.typ
let subtype mt = mt.subtype

let to_string mt =
  Type.to_string (type_of mt)
  ^ "/"
  ^ Subtype.to_string (subtype mt)

let of_string _ = assert false

let extension mt =
  match to_string mt with
  | "application/pdf" -> ".pdf"
  | "text/plain" -> ".txt"
  | "text/tab-separated-values" -> ".tsv"
  | "image/tiff" -> ".tif"
  | _ -> "" (* TODO: default to no extension should be logged *)
