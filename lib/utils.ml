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

let rename_file id new_ext filename =
  let base = Filename.remove_extension filename in
  String.concat ""
    [ base;
      "_CONVERTED";
      id;
      new_ext;
    ]

let progress_bar_rename_file_ =
  let f (a1, a2, a3) = rename_file a1 a2 a3 in
  let hush (_, _, filename) = String.take 3 filename = "utf" in
  let msg (_, new_ext, filename) =
    let basename = Filename.remove_extension filename in
    let pdf_a new_ext =
      if new_ext = ".pdf"
      then " (PDF-A)"
      else ""
    in
    String.concat ""
      [ "converting" ;
        filename ;
        " to "   ;
        basename ^ new_ext ;
        pdf_a new_ext ;
        "...\n"
      ]
  in
  Progress_bar.progress_barify f hush msg

let progress_bar_rename_file id new_ext filename =
  let echo_progress = Progress_bar.Printer.print in
  let echo_hushed_utf8 msg filename =
    if String.take 3 filename = "utf"
    then ()
    else echo_progress msg
  in
  let basename = Filename.remove_extension filename in
  let pdf_a new_ext =
    if new_ext = ".pdf"
    then " (PDF-A)"
    else ""
  in
  let msg = String.concat ""
              [ "converting" ;
                filename ;
                " to "   ;
                basename ^ new_ext ;
                pdf_a new_ext ;
                "...\n"
              ]
  in
  echo_hushed_utf8 msg filename ;
  rename_file id new_ext filename

let replace_newlines = String.replace (eol LF) (eol CRLF) << String.replace (eol CRLF) (eol LF)
