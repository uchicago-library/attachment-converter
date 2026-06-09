# attachment-converter                             -*- makefile -*-

# Attachment Converter is distributed under the terms of the GNU
# GPL-3.0-or-later.

# Copyright 2026 Matt Teichman and Nathan Mull.


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
DESTDIR ?= /usr
PROJECT_ROOT = $(shell pwd)

include $(LIB)/Makefile.gnumake
include $(LIB)/Makefile.debug

.DEFAULT_GOAL := build

################################################################################
# dev make rules

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
	rm -f $(wildcard *.maketrack)
	rm -fr opampack
.PHONY: clean

sandbox::
	eval $$(opam env)
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
.PHONY: sandbox

-include $(LIB)/Makefile.help

################################################################################
# user make rules

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
	ls -lh $(DESTDIR)/bin/attc
	cd $(PROJECT_ROOT)
	mv _build/default/main.exe $(DESTDIR)/bin/attc
	@echo Attachment Converter has been installed to $(DESTDIR)/bin/attc. 
	@echo Please ensure that $(DESTDIR)/bin is on your path.
.PHONY: install


################################################################################
# homebrew and arch specific rules

pkg-opam:
	opam init --yes --yes --disable-sandboxing
.PHONY: pkg-opam

pkg-build: pkg-opam cd-home deps
	eval $$(opam env)
	$(call DUNE,build)
	cd $(PROJECT_ROOT)
.PHONY: pkg-build

gen-man-page: opam-install
	./main.exe --help=groff > doc/attc.1


################################################################################
# releasing

ARCH_REPO_HOSTNAME = staff.lib.uchicago.edu
ARCH_REPO_PATH = /data/web/dldc/open/repos/arch
SSH_PATH = $(ARCH_REPO_HOSTNAME):$(ARCH_REPO_PATH)
VER_NUM=0.1.4

# upon version bump, this version number needs to be updated in three
# places:

# - this makefile
# - Lib.Version.ver_num
# - the PKGBUILD

TEMP_DIR := $(shell mktemp -d)

arch-release:
	scp arch/PKGBUILD $(TEMP_DIR)
	scp $(SSH_PATH)/dldc.db.tar.gz $(TEMP_DIR) || true
	scp $(SSH_PATH)/dldc.files.tar.gz $(TEMP_DIR) || true
	cd $(TEMP_DIR) && \
		makepkg -Cc && \
		repo-add -s dldc.db.tar.gz attc-$(VER_NUM)-1-x86_64.pkg.tar.zst && \
		rsync -a * $(SSH_PATH) && \
	rm -rf $(TEMP_DIR)
.PHONY: arch-release

arch-remove:
	scp $(SSH_PATH)/dldc.db.tar.gz $(TEMP_DIR)
	scp $(SSH_PATH)/dldc.files.tar.gz $(TEMP_DIR)
	repo-remove -s $(TEMP_DIR)/dldc.db.tar.gz attc
	rsync -a $(TEMP_DIR)/dldc.db.tar.gz $(SSH_PATH)
	rsync -a $(TEMP_DIR)/dldc.files.tar.gz $(SSH_PATH)
	ssh $(ARCH_REPO_HOSTNAME) rm $(ARCH_REPO_PATH)/attc-$(VER_NUM)-1-x86_64.pkg.tar.zst
	rm -rf $(TEMP_DIR)
.PHONY: arch-remove


# This file is part of Attachment Converter.

# Attachment Converter is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# Attachment Converter is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Attachment Converter. If not, see
# <https://www.gnu.org/licenses/>.
