open Mrmime
module E = Mime_type_error
module Trace = Error.T

module Type = struct
  include Content_type.Type

  let of_string s =
    Trace.of_result E.Smart.parse_err (of_string s)
end

module Subtype = struct
  include Content_type.Subtype

  let pdf = iana_exn Type.application "pdf"
  let plain = iana_exn Type.text "plain"
  let msword = iana_exn Type.application "msword"

  let docx =
    iana_exn Type.application
      "vnd.openxmlformats-officedocument.wordprocessingml.document"

  let xls = iana_exn Type.application "vnd.ms-excel"

  let xlsx =
    iana_exn Type.application
      "vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  let tsv = iana_exn Type.text "tab-separated-values"
  let gif = iana_exn Type.image "gif"
  let bmp = iana_exn Type.image "bmp"
  let tiff = iana_exn Type.image "tiff"
  let jpeg = iana_exn Type.image "jpeg"
  let of_string = assert false
end

type t = { ty : Type.t; subty : Subtype.t }

let ty mt = mt.ty
let subty mt = mt.subty
let make ty subty = { ty; subty }

let to_string mt =
  Type.to_string (ty mt)
  ^ "/"
  ^ Subtype.to_string (subty mt)

let pp = Fmt.of_to_string to_string

let of_string s =
  let out =
    Angstrom.parse_string ~consume:All
      Content_type.Decoder.content s
  in
  match out with
  | Ok { ty; subty; parameters = [] } -> Ok { ty; subty }
  | _ -> Trace.throw E.Smart.parse_err

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

let equal mt1 mt2 = compare mt1 mt2 = 0
let pdf = make Type.application Subtype.pdf
let pdfa = make Type.application Subtype.pdf
let txt = make Type.text Subtype.plain
let doc = make Type.application Subtype.msword
let docx = make Type.application Subtype.docx
let xls = make Type.application Subtype.xls
let xlsx = make Type.application Subtype.xlsx
let tsv = make Type.text Subtype.tsv
let gif = make Type.image Subtype.gif
let bmp = make Type.image Subtype.bmp
let tiff = make Type.image Subtype.tiff
let jpeg = make Type.image Subtype.jpeg
