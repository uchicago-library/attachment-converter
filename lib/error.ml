type error = Error_intf.error
type t = Error_intf.t

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
