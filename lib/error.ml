type error = Error_intf.error
type t = Error_intf.t

let to_string =
  let open Printf in
  let open Convert_error in
  function
  (* TODO: enhance these stub error messages *)
  | `MissingKey _ -> "missing key"
  | `BadMimeType (_, _) -> "bad mime type"
  | `MimeParseError -> "mime parse error"
  | `ConfigData n -> sprintf "config data: %i" n
  | `ReferParse (n, s) -> sprintf "refer parse: %i; %s" n s
  | `OcamlnetParseError
      ({ date; from; message_id }, onet_error) ->
    let unoption header_name = function
      | None -> ""
      | Some s -> sprintf "\t%s: %s\n" header_name s
    in
    sprintf
      "Email parse error!\n%s%s%s\tError Message: %s\n"
      (unoption "Date" date)
      (unoption "From" from)
      (unoption "Message-ID" message_id)
      onet_error
  | `MrmimeParseError { date; from; message_id } ->
    let unoption = function
      | None -> ""
      | Some s -> s
    in
    let _ = message_id in
    sprintf "Email parse error:\n\tDate: %s\n\tFrom: %s\n"
      (unoption date) (unoption from)
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
