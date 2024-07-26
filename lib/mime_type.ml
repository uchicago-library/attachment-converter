open Prelude
module E = Mime_type_error
module Trace = Error.T

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
  let pp = Fmt.using to_string Fmt.string
end

module Subtype = struct
  type t = { name : string }

  let to_string st = st.name
  let of_string nm = { name = String.lowercase_ascii nm }
  let pdf = of_string "pdf"
  let plain = of_string "plain"
  let msword = of_string "msword"

  let docx_subty =
    of_string
      "vnd.openxmlformats-officedocument.wordprocessingml.document"

  let xls = of_string "vnd.ms-excel"

  let xlsx =
    of_string
      "vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  let tsv = of_string "tab-separated-values"
  let gif = of_string "gif"
  let bmp = of_string "bmp"
  let tiff = of_string "tiff"
  let jpeg = of_string "jpeg"
  let pp = Fmt.using to_string Fmt.string
end

type t = { typ : Type.t; subtype : Subtype.t }

let type_of mt = mt.typ
let subtype mt = mt.subtype
let make typ subty = { typ; subtype = subty }

let to_string mt =
  Type.to_string (type_of mt)
  ^ "/"
  ^ Subtype.to_string (subtype mt)

let of_string s =
  let open Trace in
  let open E.Smart in
  let ( let* ) = Result.( >>= ) in
  let ty_str, opt_subty_str = String.cut ~sep:"/" s in
  let* subty_str = of_option parse_err opt_subty_str in
  let mt =
    make
      (Type.of_string ty_str)
      (Subtype.of_string subty_str)
  in
  Ok mt

let extension mt =
  match to_string mt with
  | "application/pdf" -> ".pdf"
  | "text/plain" -> ".txt"
  | "text/tab-separated-values" -> ".tsv"
  | "image/tiff" -> ".tif"
  | "image/bmp" -> ".bmp"
  | "image/gif" -> ".gif"
  | "image/jpeg" -> ".jpeg"
  | "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    -> ".docx"
  | "application/vnd.ms-excel" -> ".xls"
  | "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    -> ".xlsx"
  | _ -> ".unknown"
(* TODO: default to no extension should be logged *)

let compare mt1 mt2 =
  String.compare (to_string mt1) (to_string mt2)

let pdf = make Type.application Subtype.pdf
let pdfa = make Type.application Subtype.pdf
let txt = make Type.text Subtype.plain
let doc = make Type.application Subtype.msword
let docx = make Type.application Subtype.docx_subty
let xls = make Type.application Subtype.xls
let xlsx = make Type.application Subtype.xlsx
let tsv = make Type.text Subtype.tsv
let gif = make Type.image Subtype.gif
let bmp = make Type.image Subtype.bmp
let tiff = make Type.image Subtype.tiff
let jpeg = make Type.image Subtype.jpeg
