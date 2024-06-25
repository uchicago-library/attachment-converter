# attachment-converter                             -*- makefile -*-
# GNUmakefile
# Matt Teichman <https://elucidations.now.sh>

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
DUNE = opam exec -- dune $1 --display $(DISPLAY)
FREEBSDHOST = ocaml
HOME_DESTDIR = ~
DESTDIR = /usr
PROJECT_ROOT = $(shell pwd)

include $(LIB)/Makefile.gnumake
include $(LIB)/Makefile.debug

.DEFAULT_GOAL := build

################################################################################

# DEV MAKE TARGETS

all build::				## build the project binaries
	eval $$(opam env)
	$(call DUNE, build @@default)
.PHONY: build all

production release:: 			## build production binaries
	eval $$(opam env)
	$(call DUNE, build --profile release @@default)
.PHONY: production release

check test tests runtest::	## run the test suite
	eval $$(opam env)
	$(call DUNE, runtest)
.PHONY: check test tests runtest

versionedexe: versioncheck all
	cp ./_build/default/$(NAME).exe _build/default/$(NAME)-`./_build/default/$(NAME).exe --version | awk -F\\t '/^version\t/ {print $$2}'`
.PHONY: versionedexe

doc::				## build documentation
	eval $$(opam env)
	$(call DUNE, build @doc-private)
.PHONY: doc

clean: $(SUBCLEANS)		## clean up build artifacts
	eval $$(opam env)
	$(call DUNE,clean)
	rm $(wildcard *.maketrack)
.PHONY: clean

sandbox::
	eval $$(opam env)
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
.PHONY: sandbox

-include $(LIB)/Makefile.help

################################################################################

# USER MAKE TARGETS

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
	eval $$(opam env)
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
.PHONY: deps

opam-deps.maketrack: mercurial cd-home deps	
	eval $$(opam env)
	touch opam-deps.maketrack

os-deps.maketrack: opam-deps.maketrack deps
	./os-install.sh libreoffice pandoc ghostscript gnumeric vips verapdf catdoc
	touch os-deps.maketrack

shell-copy: os-deps.maketrack
	cd $(PROJECT_ROOT)
	mkdir -p ~/.config/attachment-converter/scripts
	cp $(wildcard conversion-scripts/*.sh) ~/.config/attachment-converter/scripts
.PHONY: shell-copy

opam-install::
	eval $$(opam env)
	$(call DUNE,build)
	$(call DUNE,install)
.PHONY: opam-install

home-install: shell-copy opam-install
	@eval `opam env`
	echo "Installing to $(HOME_DESTDIR)/bin/attc..."
	cp $(shell opam var bin)/attc $(HOME_DESTDIR)/bin
	ls -lh $(HOME_DESTDIR)/bin/attc
	echo
	echo "Attachment Converter has been installed to $(HOME_DESTDIR)/bin/attc."
	echo "Please ensure that $(HOME_DESTDIR)/bin is on your path."
	echo
	echo "For Mac users, run:"
	echo '    $$ echo "export PATH=~/bin:$$PATH" >> ~/.zshrc'
	echo
	echo "For WSL Debian & Arch Linux users, run:"
	echo '    $$ echo "export PATH=~/bin:$$PATH" >> ~/.bashrc'
.PHONY: home-install

install: shell-copy opam-install
	eval $$(opam env)
	@echo Installing to $(DESTDIR)/bin/attc...
	cp $(shell opam var bin)/attachment-converter $(DESTDIR)/bin/attc
	ls -lh $(DESTDIR)/bin/attc
	@echo Attachment Converter has been installed to $(DESTDIR)/bin/attc. 
	@echo Please ensure that $(DESTDIR)/bin is on your path.
	cd $(PROJECT_ROOT)
	mv _build/default/main.exe $(DESTDIR)/bin/attc
.PHONY: install
