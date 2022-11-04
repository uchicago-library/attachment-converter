module type ERROR =
sig
  type t
  val message : t -> string
end

module Constants = struct
  let meta_header_name = "X-Attachment-Converter"
  let meta_header_cont_dist = "base64"
end

let timestamp () =
  Unix.time ()
    |> string_of_float
    |> fun x -> String.(sub x 0 (length x - 1))

let renamed_file id new_ext filename =
  let base = Filename.remove_extension filename in
  String.concat ""
    [ base;
      "_CONVERTED";
      id;
      new_ext;
    ]


