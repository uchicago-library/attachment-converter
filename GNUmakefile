# attachment-converter                             -*- makefile -*-
# GNUmakefile
# Matt Teichman <https://elucidations.now.sh>

NAME = attachment-converter
OPENHTML = emacsclient -n -a "emacs --no-desktop" --eval '(eww-open-file "$1")'
LIB = makefiles
SUBCLEANS =
DISPLAY = short
DUNE = dune $1 --display $(DISPLAY)
FREEBSDHOST = ocaml
HOME_DESTDIR = ~
DESTDIR = /usr

include $(LIB)/Makefile.gnumake
include $(LIB)/Makefile.debug

.DEFAULT_GOAL := build

all build::				## build the project binaries
	$(call DUNE, build @@default)
.PHONY: build all

production release:: 			## build production binaries
	$(call DUNE, build --profile release @@default)
.PHONY: production release

check test tests runtest::	## run the test suite
	$(call DUNE, runtest)
.PHONY: check test tests runtest

versionedexe: versioncheck all
	cp ./_build/default/$(NAME).exe _build/default/$(NAME)-`./_build/default/$(NAME).exe --version | awk -F\\t '/^version\t/ {print $$2}'`
.PHONY: versionedexe

doc::				## build documentation
	$(call DUNE, build @doc-private)
.PHONY: doc

# this path is bogus but _build/default/_doc/_html/index.html is always empty
read-doc: doc			## open the documentation with $(OPENHTML)
	$(call OPENHTML, $(wildcard _build/default/_doc/_html/*/*/index.html))

clean: $(SUBCLEANS)		## clean up build artifacts
	$(call DUNE,clean)
.PHONY: clean

-include $(LIB)/Makefile.help
-include $(LIB)/Makefile.dldc

sandbox::
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
PHONY: sandbox

deps::
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
PHONY: deps

opam-install::
	$(call DUNE,build)
	$(call DUNE,install)

home-install: opam-install
	echo Installing to $(HOME_DESTDIR)/bin/attc...
	cp $(shell opam var bin)/attachment-converter $(HOME_DESTDIR)/bin/attc
	ls -lh $(HOME_DESTDIR)/bin/attc
PHONY: install

install: opam-install
	echo Installing to $(DESTDIR)/bin/attc...
	cp $(shell opam var bin)/attachment-converter $(DESTDIR)/bin/attc
	ls -lh $(DESTDIR)/bin/attc
PHONY: install
