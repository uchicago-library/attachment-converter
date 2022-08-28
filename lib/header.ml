open Prelude

module Field = struct

  module Value = struct
    module Error = struct
      type t = [
        | `ParameterParse
      ]

      let message _ = "Error reading parameters" (* TODO: more data *)
    end

    module Parameter = struct
      type t = {
        attr: string;
        value: string;
        quotes: bool;
      }

      let map_attr f p = { p with attr = f p.attr }
      let map_val f p = { p with value = f p.value }
      let map_qt f p = { p with quotes = f p.quotes }

      let attr p = p.attr
      let value p = p.value
      let quotes p = p.quotes

      let param ?(quotes=false) attr value =
        { attr = attr;
          value = value;
          quotes = quotes;
        }

      let is_quoted str = String.prefix "\"" str && String.suffix "\"" str
      let unquoted str = String.trim "\"" str
      let quoted str = "\"" ^ str ^ "\""

      let parse str =
        let cut_or_none str = match String.cut ~sep:"=" str with
          | left, Some right -> Some (left, right)
          | _, None -> None
        in
        let ( let* ) = Option.(>>=) in
        let* (attr, value) = cut_or_none str in
          Some (
            if is_quoted value then
              param ~quotes:true attr (unquoted value)
            else
              param attr value
          )

      let to_string { attr; value; quotes } =
        attr ^ "=" ^ (if quotes then quoted value else value)
    end

    type t = {
      value: string;
      params: Parameter.t list;
    }

    let value hv = hv.value
    let params hv = hv.params

    let hf_value ?(params=[]) value =
      { value = value;
        params = params;
      }

    let parse str =
      let vs = String.cuts ~sep:";" str in
      let vs = List.map String.(trim whitespace) vs in (* not sure if trimming is necessary or should be done *)
      let rec process ls =
        match ls with
        | [] -> Some []
        | None :: _ -> None
        | Some p :: ps -> Option.map (fun xs -> p :: xs) (process ps)
      in
        match vs with
        | [] -> Error `ParameterParse
        | value :: params ->
            match process (List.map Parameter.parse params) with
            | None -> Error `ParameterParse
            | Some params ->
                Ok (hf_value ~params:params value)

    let unsafe_parse = Result.get_ok << parse

    let to_string { value ; params } =
      let f curr p = curr ^ ";\n\t" ^ Parameter.to_string p in
        match params with
        | [] -> value
        | _  -> List.foldl f value params

    let lookup_param attr hv =
      let rec lookup attr ls =
        match ls with
        | [] -> None
        | p :: ps ->
            if Parameter.attr p = attr then
              Some (Parameter.value p)
            else
              lookup attr ps
      in
        lookup attr (params hv)

    let update_value new_value hv =
      { hv with value = new_value }

    let update_param attr f hv =
      let cons_op x ls = Option.either (List.snoc ls) ls x in
      let rec update ls =
        match ls with
        | [] -> cons_op (f None) []
        | p :: ps ->
            if Parameter.attr p = attr then
              cons_op (f (Some p)) ps
            else
              p :: update ps
      in
        { hv with params = update hv.params }

    let map_val attr f =
      update_param attr (Option.map (Parameter.map_val f))

    let replace_val attr new_value =
      map_val attr (k new_value)

    let remove_val attr =
      update_param attr (k None)

    let add_param ?(quotes=false) attr value =
      update_param attr
        (k (Some (Parameter.param ~quotes:quotes attr value)))
  end

  type t = {
    name : string;
    value : Value.t;
  }

  let name fld = fld.name
  let value fld = fld.value

  let h_field name value = { name = name; value = value }

  let h_field_from_str name value_str : (t, Value.Error.t) result =
    let ( let* ) = Result.(>>=) in
    let* value = Value.parse value_str in
      Ok (h_field name value)

  let to_assoc { name; value } =
    (name, Value.to_string value)

end

let from_assoc_list ls : (Field.t list, Field.Value.Error.t) result =
  let ( let* ) = Result.(>>=) in
  let f (n, v) curr =
    let* l = Field.h_field_from_str n v in
    let* r = curr in
      Ok (l :: r)
  in
    List.foldr f (Ok []) ls

let to_assoc_list ls : (string * string) list = List.map Field.to_assoc ls

let update g name fields =
  let cons_op key x ls = Option.either (fun y -> (Field.h_field key y :: ls)) ls x in
  let rec process ls =
    match ls with
    | [] -> cons_op name (g None) []
    | fld :: fs ->
        if Field.name fld = name then
          cons_op (Field.name fld) (g (Some (Field.value fld))) fs
        else
          fld :: process fs
  in
    process fields

let update_or_noop f = update (Option.map f)
let update_or_default f def = update (Option.either (f >> Option.some) (Some def))
let add new_v = update (k (Some new_v))

let lookup_param header field_name param_name =
    let ( let* ) = Option.(>>=) in
      match header # field field_name with
      | exception Not_found -> None
      | exception e -> raise e
      | hv_str ->
          let* hv = Result.to_option (Field.Value.parse hv_str) in
            Field.Value.lookup_param param_name hv
