(* Note: This module contains an interface for working with
   headers that is parsing-backend agnostic. It is NOT fully
   functional, and should NOT be used for general
   computations on headers. In particular, it does not
   follow RFC spec. See the notes below. *)

open Prelude
open Utils
module Trace = Error.T

module Field = struct
  module Value = struct
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

      (* Note: `of_string` is only called on an header value
         parameter in an email that has already been parsed,
         or on a hand-built header value parameter.
         Therefore, we assume it never fails. This does NOT
         correctly fit the specification in RFC 2045. *)
      let of_string str =
        let cut str =
          let open String in
          match cut ~sep:"=" str with
          | left, Some right ->
            (trim whitespace left, trim whitespace right)
          | left, None -> (trim whitespace left, "")
        in
        uncurry make (cut str)

      let to_string { attr; value; quotes } =
        attr ^ "=" ^ if quotes then quoted value else value
    end

    type t = { value : string; params : Parameter.t list }

    let value hv = hv.value
    let params hv = hv.params
    let make ?(params = []) value = { value; params }

    (* Note: see the note for `Parameter.of_string` above *)
    let of_string str =
      let vs = String.cuts ~sep:";" str in
      (* Note: according to RFC 2045, trimming is necessary
         because whitespace is not allowed in attributes or
         values. *)
      let vs = List.map String.(trim whitespace) vs in
      match List.map String.(trim whitespace) vs with
      | [] -> make ""
      | v :: ps ->
        make ~params:(List.map Parameter.of_string ps) v

    (* Note: we hardcode CRLFs into the output of
       `to_string` because it is never used in a full
       serialized email, it is only passed into the email
       parsing backend. *)
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
  end

  type t = { name : string; value : Value.t }

  let name fld = fld.name
  let value fld = fld.value
  let make name value = { name; value }

  (* Note: See the note for `Parameter.of_string` above *)
  let of_string str =
    let name, optValueStr = String.cut ~sep:":" str in
    let name = String.(trim whitespace) name in
    let valueStr = Option.default "" optValueStr in
    let value = Value.of_string valueStr in
    make name value

  let to_string fld =
    name fld ^ ": " ^ Value.to_string (value fld)

  let to_assoc { name; value } =
    (name, Value.to_string value)
end

type t = Field.t list

let of_list l = l

let to_assoc_list ls : (string * string) list =
  List.map Field.to_assoc ls

(* Note: See the note for `Value.to_string` above. *)
let to_string fld =
  String.join ~sep:"\r\n" (List.map Field.to_string fld)
  ^ "\r\n"
