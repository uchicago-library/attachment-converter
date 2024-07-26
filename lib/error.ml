type error = Error_intf.error
type t = Error_intf.t

let to_string =
  let open Printf in
  function
  (* TODO: enhance these stub error messages *)
  | `MissingKey _ -> "missing key"
  | `BadMimeType (_, _) -> "bad mime type"
  | `MimeParseError -> "mime parse error"
  | `ConfigData n -> sprintf "config data: %i" n
  | `ReferParse (n, s) -> sprintf "refer parse: %i; %s" n s
  | `OcamlnetParseError _ -> "email parse error\n"
  | `MrmimeParseError _ -> "email parse error\n"
  | `NotInstalled lis ->
    "Attachment Converter will not run unless all of its \
     OS-level dependencies are installed.\n\n\
     It looks like the following software packages still \
     need to be installed:\n\
     \t"
    ^ String.concat "\n\t" (List.map Package.getName lis)
    ^ "\n\r"
  | `UnsupportedOS os ->
    os
    ^ " is not a supported operating system for Attachment \
       Converter.\n\
       Here is a list of supported Os-es:\n\n\
       \tmacOS\n\
       \tArch Linux\n\
       \tWSL Debian\n\
       \r"

module T = struct
  let with_error err x =
    let coerced = (err : [< error] :> error) in
    match x with
    | Ok _ -> x
    | Error errs -> Error (coerced :: errs)

  let new_list err =
    let coerced = (err : [< error] :> error) in
    [ coerced ]

  let new_error err = Error (new_list err)
  let throw = new_error

  let of_option err x =
    match x with
    | Some x -> Ok x
    | None -> throw err
end
