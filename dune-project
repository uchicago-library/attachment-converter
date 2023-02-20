(lang dune 2.7)

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
  (qtest :dev)
  (utop :dev)
  (ocamlformat :dev)
  (ocp-index :dev)
  (merlin :dev)))
