open Prelude

type t =
  | Body
  | Attachment
  | Message of t
  | Multipart of t option list

let sender =
  let open Mrmime.Mailbox in
    Local.[ w "nobody"; w "important" ]
    @ Domain.(domain, [ a "gmail"; a "com" ])

let reciever =
  let open Mrmime.Mailbox in
    Local.[ w "somebody"; w "else" ]
    @ Domain.(domain, [ a "gmail"; a "com" ])

let date =
  let open Mrmime in
  let now = Option.get (Ptime.of_float_s 1619454050.0) in
    Date.of_ptime ~zone:Date.Zone.GMT now

let subject =
  let open Mrmime.Unstructured.Craft in
    compile
      [ v "An";
        sp 1;
        v "Inquiry";
        sp 1;
        v "About";
        sp 1;
        v "Nothing" ]

let header =
  let open Mrmime in
  let open Mrmime.Header in
    empty
    |> add Field_name.date Field.(Date, date)
    |> add Field_name.subject Field.(Unstructured, subject)
    |> add Field_name.from Field.(Mailboxes, [ sender ])
    |> add (Field_name.v "To")
        Field.(Addresses, Address.[ mailbox reciever ])

let content_disposition = Mrmime.Field_name.v "Content-Disposition"

let body_mime_type =
  Mrmime.Content_type.(
    make `Text (Subtype.v `Text "plain") Parameters.empty
  )

let attachment_mime_type =
  Mrmime.Content_type.(
    make `Image (Subtype.v `Image "png") Parameters.empty
  )

let message_mime_type =
  Mrmime.Content_type.(
    make `Message (Subtype.v `Message "rfc822") Parameters.empty
  )

let multipart_mime_type =
  Mrmime.Content_type.(
    make `Multipart (Subtype.v `Multipart "mixed")
      (Parameters.singleton
        (Parameters.k "boundary")
        (Parameters.v "letsmakeaboundary"))
  )

let body_header =
  let open Mrmime in
  let open Mrmime.Header in
    empty
    |> add Field_name.content_type Field.(Content, body_mime_type)
    |> add Field_name.content_encoding Field.(Encoding, `Bit7)

let attachment_header =
  let open Mrmime in
  let open Mrmime.Header in
    empty
    |> add Field_name.content_type Field.(Content, attachment_mime_type)
    |> add Field_name.content_encoding Field.(Encoding, `Base64)
    |> add content_disposition
        Field.
          ( Unstructured,
            Unstructured.Craft.(
              compile
                [ v "attachment";
                  sp 0;
                  v ";";
                  sp 1;
                  v "filename";
                  sp 0;
                  v "=";
                  sp 0;
                  v "thisisnotthefileyouarelookingfor.png"
                ]) )

let multipart_header =
  let open Mrmime in
  let open Mrmime.Header in
    empty
    |> add Field_name.content_type Field.(Content, multipart_mime_type)

let body =
"To whom it may concern,

I would like to inquire about nothing in particular. It is not
presently urgent that you respond in a timely manner, nor does
your imminent response bear any weight in on the current state
of affairs.

yours truely and eternally,
noboby important"

let attachment_data =
  "THISISNOTACTUALLYDATABUTWECANPRETENTTHATITISFORTHESAKEOFARGUMENT"

let message_header =
  let open Mrmime in
  let open Mrmime.Header in
  empty
  |> add Field_name.content_type Field.(Content, message_mime_type)
  |> add Field_name.content_encoding Field.(Encoding, `Bit7)

let to_mrmime_tree opt_s =
  let open Mrmime.Mail in
  let open Mrmime.Header in
  let rec go f s =
    match s with
    | Some Body ->
        f body_header, Some (Leaf body)
    | Some Attachment ->
        f attachment_header, Some (Leaf attachment_data)
    | Some (Message msg) ->
        let (h, b) = go id (Some msg) in
          f message_header, Some (Message (h, Option.get b))
    | Some (Multipart parts) ->
        f multipart_header, Some (Multipart (List.map (go id) parts))
    | None -> f empty, None
  in
    go (concat header) opt_s

let to_email =
  Serialize.(to_mrmime_tree >> make >> to_string)

let to_string =
  let rec go pre top extend skel =
    let head_pre = pre ^ if top then "" else "|-- " in
    let rest_pre = pre ^ if top
                         then ""
                         else if extend
                              then "|   "
                              else "    " in
    let rest_go = go rest_pre (top && false) in
      match skel with
      | Some Body -> head_pre ^ "Body"
      | Some Attachment -> head_pre ^ "Attachment"
      | Some (Message sk) -> head_pre ^ "Message\n" ^ rest_go false (Some sk)
      | Some (Multipart parts) ->
          head_pre ^ "Multipart\n" ^
          String.join ~sep:"\n" (List.map (rest_go true) parts) ^
          "\n" ^ rest_pre
      | None -> head_pre ^ "Header Only"
  in
    go "" true false

module Mrmime_Skeleton = struct
  open Convert.Mrmime_parsetree

  let rec to_skeleton tree =
    let open Mrmime.Mail in
    let (_, opt_t) = tree in
      match opt_t with
      | Some (Leaf _) ->
          if is_attachment tree
          then Some Attachment
          else Some Body
      | Some (Message (h, b)) -> Some (Message (Option.get (to_skeleton (h, Some b))))
      | Some (Multipart parts) -> Some (Multipart (List.map to_skeleton parts))
      | None -> None
end

