(lang dune 3.7)

(generate_opam_files true)

(authors "Matt Teichman")
(maintainers "teichman@uchicago.edu")

(package
 (name attachment-converter)
 (synopsis "does something")
 (description "cf. synopsis")
 (depends
  (ocaml (>= 4.12.0))
  dune
  prelude
  ocamlbuild
  ocamlfind
  mrmime
  ocamlnet
  camlp-streams
  ounit2
  cmdliner
  utop
  ocamlformat-rpc
  ocaml-lsp-server
  odoc
  (qtest :dev)
  (ocamlformat :dev)
  (ocp-index :dev)
  (merlin :dev)))
