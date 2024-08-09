open Prelude

let message msgs =
  String.join ~sep:"" (map Error.to_string msgs)

let debug es =
  let to_string e =
    match e with
    | #Config_entry_error.t as e ->
      Config_entry_error.debug e
    | #Mime_type_error.t as e -> Mime_type_error.debug e
    | #Formats_error.t as e -> Formats_error.debug e
    | _ -> "TODO"
  in
  String.join ~sep:"\n"
    ([ "Error Trace:" ] @ List.map to_string es)
