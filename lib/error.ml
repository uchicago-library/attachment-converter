type error = Error_intf.error
type t = Error_intf.t

let to_string =
  let open Printf in
  let open Convert_error in
  function
  | `MissingKey k ->
    sprintf "Missing key: %s\n" (Config_key.to_string k)
  | `BadMimeType (mime_string, k) ->
    sprintf
      "Bad mime type string: '%s', in configuration field \
       '%s'\n"
      mime_string
      (Config_key.to_string k)
  | `MimeParseError -> "Mime parse error!"
  | `ConfigData n ->
    sprintf "Config file error: line %i\n" n
  | `ReferParse (n, s) ->
    sprintf
      "Refer parse error in config file, line %i\n\
       Content of line: %s" n s
  | `OcamlnetParseError
      ({ date; from; message_id }, onet_error) ->
    let unoption header_name = function
      | None -> ""
      | Some s -> sprintf "%s: %s\n" header_name s
    in
    sprintf
      "Error-Type: Email parse error\n\
       %s%s%sError-Message: %s\n\n"
      (unoption "Date" date)
      (unoption "From" from)
      (unoption "Message-ID" message_id)
      onet_error
  | `MrmimeParseError { date; from; message_id } ->
    let unoption header_name = function
      | None -> ""
      | Some s -> sprintf "%s: %s\n" header_name s
    in
    sprintf "Error-Type: Email parse error!\n%s%s%s\n\n"
      (unoption "Date" date)
      (unoption "From" from)
      (unoption "Message-ID" message_id)
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
       \tWSL Ubuntu\n\
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

  let of_result err x =
    match x with
    | Ok x -> Ok x
    | Error _ -> throw err
end
