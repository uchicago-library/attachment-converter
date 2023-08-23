open Prelude

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

  let application = Application
  let text = Text
  let image = Image
end

module Subtype = struct
  type t = { name : string }

  let to_string st = st.name
  let of_string name = { name = name }

  let pdf = of_string "pdf"
  let plain = of_string "plain"
  let msword = of_string "msword"
  let docx_subty = of_string "vnd.openxmlformats-officedocument.wordprocessingml.document"
  let excel = of_string "vnd.ms-excel"
  let tsv = of_string "tab-separated-values"
  let gif = of_string "gif"
  let bmp = of_string "bmp"
  let tiff = of_string "tiff"
  let jpeg = of_string "jpeg"
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

let make typ subty =
  { typ = typ ;
    subtype = subty ;
  }

let to_string mt =
  Type.to_string (type_of mt)
  ^ "/"
  ^ Subtype.to_string (subtype mt)

let of_string s =
  let ( let* ) = Result.(>>=) in
  let ty_str, opt_subty_str = String.cut ~sep:"/" s in
  let* subty_str = Result.of_option `MimeType opt_subty_str in
  let mt = make (Type.of_string ty_str) (Subtype.of_string subty_str) in
  Ok mt

let extension mt =
  match to_string mt with
  | "application/pdf" -> ".pdf"
  | "text/plain" -> ".txt"
  | "text/tab-separated-values" -> ".tsv"
  | "image/tiff" -> ".tif"
  | _ -> "" (* TODO: default to no extension should be logged *)

let compare mt1 mt2 = String.compare (to_string mt1) (to_string mt2)

let pdf = make Type.application Subtype.pdf
let pdfa = make Type.application Subtype.pdf
let txt = make Type.text Subtype.plain
let doc = make Type.application Subtype.msword
let docx = make Type.application Subtype.docx_subty
let xls = make Type.application Subtype.excel
let tsv = make Type.text Subtype.tsv
let gif = make Type.image Subtype.gif
let bmp = make Type.image Subtype.bmp
let tiff = make Type.image Subtype.tiff
let jpeg = make Type.image Subtype.jpeg
