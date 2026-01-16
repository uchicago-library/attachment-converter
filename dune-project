(lang dune 3.20)

(generate_opam_files true)

(authors "Matt Teichman" "Nathan Mull" "Emily Schartz" "Cormac Duhamel" "Owen Price-Skelly" "Charles Njoroge" "Nishchay Karle" "Obi Obetta" "Ben Kim")
(maintainers "teichman@uchicago.edu")

(package
 (name attachment-converter)
 (synopsis "Converts email attachments to preservation formats")
 (description "
Attachment Converter (attc) is a command-line tool for converting
email attachments in an MBOX to preservation formats.  Whenever it
finds a file attachment in the input MBOX that it knows how to
convert, it performs the conversion and inserts the new file
attachment back into the email next to the original, to make it easy
for the user locate.

Attachment Converter calls out to external utilities to perform all of
its file conversions, but the user can also configure it to convert a
new format of their of choice using a utility of their choice, as long
as that utility has the ability to be run on the UNIX command line.

Attachment Converter reads from stdin and outputs to stdout, with
error messages getting logged to stderr.
")
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
  re
  (qtest :dev)
  (ocamlformat :dev)
  (ocp-index :dev)
  (merlin :dev)))

(cram enable)
