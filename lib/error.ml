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
  | `EmailParseError -> "email parse error"
  | `NotInstalled _ -> "not installed"
  | `UnsupportedOS os -> sprintf "unsupported os: %s" os

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
