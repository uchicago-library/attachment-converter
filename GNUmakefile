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
SUBCLEANS = # what is this for?
DISPLAY = short
EVAL = eval $$(opam env)
DUNE = $(EVAL); opam exec -- dune $1 --display $(DISPLAY)
FREEBSDHOST = ocaml # what is this for?
HOME_DESTDIR = ~
DESTDIR = /usr
PROJECT_ROOT = $(shell pwd)

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

clean: $(SUBCLEANS)		## clean up build artifacts
	$(call DUNE,clean)
	rm $(wildcard *.maketrack)
.PHONY: clean

sandbox::
	$(EVAL); opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
.PHONY: sandbox

-include $(LIB)/Makefile.help

################################################################################

opam:
	./os-install.sh opam
	opam init --yes --yes --disable-sandboxing
.PHONY: opam

mercurial: opam
	./os-install.sh mercurial
.PHONY: mercurial

cd-home:
	cd $(PROJECT_ROOT)

deps::
	$(EVAL); opam repository add dldc https://dldc.lib.uchicago.edu/opam
	$(EVAL); opam install . --deps-only --yes
.PHONY: deps

opam-deps.maketrack: mercurial cd-home deps	
	$(EVAL); touch opam-deps.maketrack

os-deps.maketrack: opam-deps.maketrack deps
	./os-install.sh libreoffice pandoc ghostscript gnumeric vips verapdf catdoc
	touch os-deps.maketrack

shell-copy: os-deps.maketrack
	cd $(PROJECT_ROOT)
	mkdir -p ~/.config/attachment-converter/scripts
	cp $(wildcard conversion-scripts/*.sh) ~/.config/attachment-converter/scripts
.PHONY: shell-copy

opam-install::
	$(EVAL); $(call DUNE,build)
	$(EVAL); $(call DUNE,install)
.PHONY: opam-install

home-install: shell-copy opam-install
	@echo Installing to $(HOME_DESTDIR)/bin/attc...
	$(EVAL); cp $(shell opam var bin)/attc $(HOME_DESTDIR)/bin
	ls -lh $(HOME_DESTDIR)/bin/attc
	@echo Attachment Converter has been installed to $(HOME_DESTDIR)/bin/attc. 
	@echo Please ensure that $(HOME_DESTDIR)/bin is on your path.
.PHONY: home-install

install: shell-copy opam-install
	@echo Installing to $(DESTDIR)/bin/attc...
	$(EVAL); cp $(shell opam var bin)/attachment-converter $(DESTDIR)/bin/attc
	ls -lh $(DESTDIR)/bin/attc
	@echo Attachment Converter has been installed to $(DESTDIR)/bin/attc. 
	@echo Please ensure that $(DESTDIR)/bin is on your path.

	cd $(PROJECT_ROOT)
	mv _build/default/main.exe $(DESTDIR)/bin/attc
.PHONY: install

################################################################################

clone-repo:
	@echo Cloning attc git repo...
	cd $(HOME_DESTDIR)
	mkdir attachment-converter
	cd attachment-converter
	git clone https://github.com/uchicago-library/attachment-converter.git
.PHONY: home-install

# Formerly in get-started.sh
# opam switch create 4.12.0 &&
# eval $(opam env) &&
# opam install -y ocamlbuild ocamlfind dune mrmime ocamlnet &&
# opam pin -y add -k hg prelude https://www.lib.uchicago.edu/keith/hg/prelude &&
# opam pin -y add mattlude https://github.com/bufordrat/mattlude.git &&
# opam pin -y add spinup https://github.com/bufordrat/spinup.git &&
# opam list

# Try these out
# make all
# make build
# make all build
