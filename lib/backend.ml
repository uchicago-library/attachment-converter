type t = Mrmime | Ocamlnet

let to_string = function
  | Ocamlnet -> "OCamlnet"
  | Mrmime -> "Mr. Mime"
