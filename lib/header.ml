open Prelude
open Utils

module Field = struct
  module Value = struct
    module Error = struct
      type t = [`ParameterParse | `ValueParse]

      let message _ =
        "Error reading parameters" (* TODO: more data *)
    end

    module Parameter = struct
      type t =
        { attr : string; value : string; quotes : bool }

      let attr p = p.attr

      let value ?(quotes = false) p =
        if quotes && p.quotes
        then quoted p.value
        else p.value

      let quotes p = p.quotes

      let make ?(quotes = false) attr value =
        { attr;
          value = unquoted value;
          quotes = is_quoted value || quotes
        }

      let map_attr f p = { p with attr = f p.attr }
      let map_value f p = { p with value = f p.value }

      let of_string str =
        let cut_or_none str =
          match String.cut ~sep:"=" str with
          | left, Some right -> Some (left, right)
          | _, None -> None
        in
        Option.map (uncurry make) (cut_or_none str)

      let to_string { attr; value; quotes } =
        attr ^ "=" ^ if quotes then quoted value else value
    end

    type t = { value : string; params : Parameter.t list }

    let value hv = hv.value
    let params hv = hv.params
    let make ?(params = []) value = { value; params }

    let of_string str =
      let vs = String.cuts ~sep:";" str in
      let vs = List.map String.(trim whitespace) vs in
      (* not sure if trimming is necessary or should be
         done *)
      let rec process ls =
        match ls with
        | [] -> Some []
        | None :: _ -> None
        | Some p :: ps ->
          Option.map (fun xs -> p :: xs) (process ps)
      in
      match vs with
      | [] -> Error `ParameterParse
      | value :: params -> (
        match
          process (List.map Parameter.of_string params)
        with
        | None -> Error `ParameterParse
        | Some params -> Ok (make ~params value) )

    let to_string { value; params } =
      let f curr p =
        curr ^ ";\r\n\t" ^ Parameter.to_string p
      in
      match params with
      | [] -> value
      | _ -> List.foldl f value params

    let lookup_param ?(quotes = false) attr hv =
      let rec lookup attr ls =
        match ls with
        | [] -> None
        | p :: ps ->
          if Parameter.attr p = attr
          then Some (Parameter.value ~quotes p)
          else lookup attr ps
      in
      lookup attr (params hv)

    let update_value new_value hv =
      { hv with value = new_value }

    let update_param attr f hv =
      let cons_op x ls =
        Option.either (List.snoc ls) ls x
      in
      let rec update ls =
        match ls with
        | [] -> cons_op (f None) []
        | p :: ps ->
          if Parameter.attr p = attr
          then cons_op (f (Some p)) ps
          else p :: update ps
      in
      { hv with params = update hv.params }

    (* Note: updates in the case that the attribute already
       appears as a paramater. *)
    let add_param ?(quotes = false) attr value =
      update_param attr
        (k (Some (Parameter.make ~quotes attr value)))
  end

  type t = { name : string; value : Value.t }

  let name fld = fld.name
  let value fld = fld.value
  let make name value = { name; value }

  let of_string str =
    let ( let* ) = Result.( >>= ) in
    let name, optValueStr = String.cut ~sep:":" str in
    let name = String.(trim whitespace) name in
    let* valueStr =
      Option.(
        to_result
          (map String.(trim whitespace) optValueStr)
          ~none:`ValueParse )
    in
    let* value = Value.of_string valueStr in
    Ok (make name value)

  let to_string fld =
    name fld ^ ": " ^ Value.to_string (value fld)

  let to_assoc { name; value } =
    (name, Value.to_string value)
end

type t = Field.t list

let to_list l = l
let of_list l = l

let of_assoc_list ls :
    (Field.t list, Field.Value.Error.t) result =
  let ( let* ) = Result.( >>= ) in
  let f (n, v) curr =
    let* l = Field.of_string (n ^ ": " ^ v) in
    let* r = curr in
    Ok (l :: r)
  in
  List.foldr f (Ok []) ls

let to_assoc_list ls : (string * string) list =
  List.map Field.to_assoc ls

let to_string fld =
  String.join ~sep:"\r\n" (List.map Field.to_string fld)
  ^ "\r\n"

let update g name hd =
  let cons_op key x ls =
    Option.either (fun y -> Field.make key y :: ls) ls x
  in
  let rec process ls =
    match ls with
    | [] -> cons_op name (g None) []
    | fld :: fs ->
      if Field.name fld = name
      then
        cons_op (Field.name fld)
          (g (Some (Field.value fld)))
          fs
      else fld :: process fs
  in
  process hd
