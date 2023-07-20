open Prelude

module type ERROR =
sig
  type t
  val message : t -> string
end

module Constants = struct
  let meta_header_name = "X-Attachment-Converter"
  let meta_header_cont_dist = "base64"
end

let is_quoted str = String.prefix "\"" str && String.suffix "\"" str
let unquoted str = String.trim "\"" str
let quoted str = "\"" ^ str ^ "\""

let timestamp () =
  Unix.time ()
    |> string_of_float
    |> fun x -> String.(sub x 0 (length x - 1))

let rename_file id new_ext filename pbar =
  let base = Filename.remove_extension filename in
  let new_filename =
    String.concat ""
      [ base;
        "_CONVERTED";
        id;
        new_ext;
      ]
  in
  let () =
    if String.take 3 filename = "utf"
    then ()
    else
      let pdf_a new_ext =
        if new_ext = ".pdf"
        then " (PDF-A)"
        else ""
      in
      let msg = String.concat ""
        [ "Converting " ;
          filename ;
          " to "   ;
          new_filename ;
          pdf_a new_ext ;
          "..."
        ]
    in
    Progress_bar.Printer.print msg pbar
  in
  new_filename

let replace_newlines = String.replace (eol LF) (eol CRLF) << String.replace (eol CRLF) (eol LF)
