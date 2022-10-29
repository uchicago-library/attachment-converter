(* signature for errors as handled throughout this program *)

module type ERROR =
sig
  type t
  val message : t -> string
end

module _ : ERROR = Configuration.ParseConfig.Error
module _ : ERROR = Header.Field.Value.Error
module _ : ERROR with
  type t = [
    `EmailParse
  ] = Convert.Ocamlnet_parsetree.Error

module Error : ERROR with
  type t = [
    | Convert.Ocamlnet_parsetree.Error.t
    | Configuration.ParseConfig.Error.t
  ] = struct
  type t = [
    | Convert.Ocamlnet_parsetree.Error.t
    | Configuration.ParseConfig.Error.t
  ]

  let message err =
    match err with
    | #Convert.Ocamlnet_parsetree.Error.t as e ->
        Convert.Ocamlnet_parsetree.Error.message e
    | #Configuration.ParseConfig.Error.t as e ->
        Configuration.ParseConfig.Error.message e
end
