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

  let return = Result.ok
  let bind = Result.bind
  let throw = new_error

  let catch x f =
    let open Prelude.Result in
    x >>/ fun e -> witherr (fun tr -> tr @ e) (f e)
  (* match x with *)
  (* | Ok v -> Ok v *)
  (* | Error e -> *)
  (*   Result.map_error *)
  (*     (fun tr -> tr @ e) *)
  (*     (f e) *)

  (* let with_error err x = *)
  (*   let coerced = (err : [< error] :> error) in *)
  (*   catch x (fun _ -> throw coerced) *)

  let of_option err x =
    match x with
    | Some x -> Ok x
    | None -> throw err
end
