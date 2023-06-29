# attachment-converter                             -*- makefile -*-
# GNUmakefile
# Matt Teichman <https://elucidations.now.sh>
#!/bin/bash
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c #need this? pt 2
# IFS=$'\n\t'
.ONESHELL:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

NAME = attachment-converter
LIB = makefiles
SUBCLEANS =
DISPLAY = short
DUNE = dune $1 --display $(DISPLAY)
FREEBSDHOST = ocaml
HOME_DESTDIR = ~
DESTDIR = /usr
OS_INSTALL =
OS_DEPS = libreoffice pandoc ghostscript

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

builddeps.maketrack:
	ifeq [brew --version]
		$(OS_INSTALL) = brew install
	ifeq [pacman --version]
		$(OS_INSTALL) = pacman -S
	ifeq [apt --version]
		$(OS_INSTALL) = apt install
	endif
	echo --package manager command is $(OS_INSTALL)--
	
	$(OS_INSTALL) opam
	touch builddeps.maketrack
	
rundeps.maketrack: builddeps.maketrack
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
	touch rundeps.maketrack

clean: $(SUBCLEANS)		## clean up build artifacts
	$(call DUNE,clean)
	rm $(wildcard *.maketrack)
.PHONY: clean

-include $(LIB)/Makefile.help

sandbox::
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
	eval $(opam env)
.PHONY: sandbox

opam-install::
	$(call DUNE,build)
	$(call DUNE,install)
.PHONY: opam-install

home-install: opam-install 
	ifeq [pacman --version]
		$(OS_INSTALL) $(OS_DEPS) libvips
	else
		$(OS_INSTALL) $(OS_DEPS) vips verapdf
	endif

	echo Cloning attc git repo...
	cd $(HOME_DESTDIR)
	mkdir attachment-converter
	cd attachment-converter
	git clone https://github.com/uchicago-library/attachment-converter.git

	echo Copying shell scripts...
	cd $(HOME_DESTDIR)/attachment-converter
	mkdir -p ~/.config/attachment-converter/scripts
	cp conversion-scripts/*.sh ~/.config/attachment-converter/scripts

	echo Installing to $(HOME_DESTDIR)/bin/attc...
	cp $(shell opam var bin)/attachment-converter $(HOME_DESTDIR)/bin/attc
	ls -lh $(HOME_DESTDIR)/bin/attc
	echo Attachment Converter has been installed to $(HOME_DESTDIR)/bin/attc. 
	echo Please ensure that $(HOME_DESTDIR)/bin is on your path.
.PHONY: home-install

install: opam-install
	echo Installing to $(DESTDIR)/bin/attc...
	cp $(shell opam var bin)/attachment-converter $(DESTDIR)/bin/attc
	ls -lh $(DESTDIR)/bin/attc
	echo Attachment Converter has been installed to $(DESTDIR)/bin/attc. 
	echo Please ensure that $(DESTDIR)/bin is on your path.
.PHONY: install

# Formerly in get-started.sh
# opam switch create 4.12.0 &&
# eval $(opam env) &&
# opam install -y ocamlbuild ocamlfind dune mrmime ocamlnet &&
# opam pin -y add -k hg prelude https://www.lib.uchicago.edu/keith/hg/prelude &&
# opam pin -y add mattlude https://github.com/bufordrat/mattlude.git &&
# opam pin -y add spinup https://github.com/bufordrat/spinup.git &&
# opam list
