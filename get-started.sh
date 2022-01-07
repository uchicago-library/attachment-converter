#!/bin/sh
opam switch create 4.12.0 &&
eval $(opam env) &&
opam install ocamlbuild ocamlfind dune &&
opam pin add -k hg prelude https://www.lib.uchicago.edu/keith/hg/prelude &&
opam pin add mattlude https://github.com/bufordrat/mattlude.git &&
opam pin add spinup https://github.com/bufordrat/spinup.git

