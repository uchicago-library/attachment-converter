module REPLTesting = struct

  (* for reference, MBOX From line:
   * From root@gringotts.lib.uchicago.edu Fri Jan 21 11:48:27 2022 *)

  include Conversion_ocamlnet

  let unparts_opt = function
    | Ok (_, `Parts lst) -> lst
    | _ -> assert false

  let get_header = function
    | (hd, _) -> hd

  let parse_file str = 
    Configuration.ParseConfig.parse_config_file str

  let print_error err = Printf.printf "%s\n" (Error.message err)

  let tree () = let pdf_h = Netmime.basic_mime_header ["content-type", "application/pdf"] in
    let pdf_data = Unix.Proc.read ["cat"; "/Users/cormacduhamel/Downloads/Nietzsche.pdf"] in
    (pdf_h, `Parts [(pdf_h, `Body (Netmime.memory_mime_body pdf_data))])

  let err_tree () = let pdf_h = Netmime.basic_mime_header ["content-type", "application/pdf"] in
  let txt_h = Netmime.basic_mime_header ["content-type", "text/plain"] in
  let pdf_data = Unix.Proc.read ["cat"; "/Users/cormacduhamel/Downloads/Nietzsche.pdf"] in
  (pdf_h, `Parts [(pdf_h, `Body (Netmime.memory_mime_body pdf_data)); (txt_h, `Body (Netmime.memory_mime_body "str_data"))])

  let dict () = 
    parse_file "/Users/cormacduhamel/sample_refer.txt"

  let test_acopy () =
    let ( let* ) = Result.(>>=) in
    let tree = tree () in
    let* dict = Result.witherrc (`DummyError) (dict ()) in
    acopy dict tree

  (** convenience function for unwrapping a `Parts; for REPL only *)
  let unparts = function
    | `Parts plist -> plist
    | _ -> assert false

  (** convenience function for unwrapping a `Body; for REPL only *)
  let unbody = function
    | `Body b -> b
    | _ -> assert false

  let to_mbox ?(escape=false) ?(eol="\n") =
    let fromline = "From jorge@babel.lib Thu Aug 24 12:00:00 1899" ^ eol in
    if escape then
      let open Strings in
      let escape_froms = replace (eol ^ "From ") (eol ^ ">From ") << replace (eol ^ ">From ") (eol ^ ">>From ") in
      fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) "" << map escape_froms
    else
      fold_left (fun acc email -> acc ^ fromline ^ email ^ eol) ""
         
  let get_body = function
  | (_, bd) -> unbody bd
    
  (** quick access to the PDF attachment part of our example Christmas
      tree email *)
  let xmas_tree () =
    let _, parts = Result.get_ok (parse (readfile "2843")) in
    match unparts parts with
      _ :: attached :: _ -> attached
    | _ -> assert false

  (** function to change the mime type to PDF *)
  let header_func hstring =
    let hs = String.lowercase_ascii hstring in
    match Strings.(
      (substr "content-disposition: attachment;" hs,
       substr "content-type: application/vnd.openxmlformats-officedocument.wordprocessingml.document" hs
      )
          )
    with
      (Some _, Some _) -> update_mimetype
                            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                            "application/pdf"
                            (update_both_filenames ~ext:".pdf" hstring)
    | _ -> hstring (* noop when not a pdf and attachment *)

  (** constant function that returns an example PDF-A as an output;
      assumes you have a file by that name in your project that is a
      PDF-A *)
  let body_func _ = readfile "xmas-PDFA.pdf"

  (** function from filepath pointing at input email to output email
      as a string *)
  (* let docx_convert_test fname =
    let tree = parse (readfile fname) in
    let converted_tree = acopy header_func body_func tree in
    to_string converted_tree *)

  (* let upcase_header_and_delete_body fname =
    let f = String.uppercase_ascii in
    let g = fun _ -> "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_upcased_and_deleted") (tree |> (amap f g) |> to_string)

  let omit_gore_y_details fname =
    let f = Fun.const "Content-Type: application/json\r\n\r\n" in
    (* let f = fun _ -> "From: Bill Clinton <president@whitehouse.gov>\r\n\r\n" in *)
    let g = Fun.const "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_contented") (tree |> (amap f g) |> to_string)

  (** takes filepath as input and writes a new file with extra spaces
      in the headers *)
  let extra_spaces_in_header fname =
    let double_space c = if c == ' ' then "  " else String.make 1 c in
    let f s = s |> String.foldr (fun c l -> double_space c :: l) [] |> String.concat "" in
    let tree = parse (Prelude.readfile fname) in
    Prelude.writefile ~fn:(fname ^ "_extra_spaces_in_header") (tree |> (amap f id) |> to_string)
  (* Not sure if this should be possible, may throw an execption *) *)
end
