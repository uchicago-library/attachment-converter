# attachment-converter                             -*- makefile -*-

# Attachment Converter is distributed under the terms of the GNU
# GPL-3.0-or-later.

# Copyright 2026 Matt Teichman and Nathan Mull.


SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c #need this? pt 2
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

SOLVER =

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
	opam install . $(if $(SOLVER),--solver=$(SOLVER),) --deps-only --yes -vv --debug-level=3
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
	cd $(PROJECT_ROOT)
	sudo mv _build/default/main.exe $(DESTDIR)/bin/attc
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
STAFF_LIB_HOSTNAME = $(ARCH_REPO_HOSTNAME)
STAFF_LIB_PATH = /data/web/dldc/opam/packages/prelude
PRELUDE_VER_NUM = 100.7
PRELUDE_OPAM_PATH = $(STAFF_LIB_HOSTNAME):$(STAFF_LIB_PATH)/prelude.$(PRELUDE_VER_NUM)
VER_NUM = 0.1.44
DEBIAN_CODENAME = resolute

# upon version increment, do the following steps slash update the version number in the following places:

# - this makefile (i.e. the value of VER_NUM)
# - Lib.Version.ver_num
# - the PKGBUILD
# - the debian changelog
# - make sure the changelog mentions the current version of Debian, e.g. `attachment-converter (0.1.43-1~resolute) resolute`
# - `date -R` to emit the current timestamp in the form the changelog wants
# - the formula in the homebrew-attc repository (name of tarball in url field)
# - the git tag for the new release
# - if the set of opam third-party packages has changed since the last release, run `make opampack` to refresh the USER_PACKAGES and PACKAGES variables in OpamPack.sh
# - check that latest Prelude is in debian directory; update with opam file from DLDC repo
# - check that ubuntu_wsl/OpamPack.sh is copying the right Prelude path (e.g. not prelude.100.1 anymore)


# also: make sure the prelude checksum in
# ubuntu_wsl/prelude.100.7/opam is up to date

# AND THEN update the sha checksum in the homebrew formula once the
# new release tag is uploaded

# to publish the package to the DLDC Arch Linux repo: `make arch-release`

TEMP_DIR := $(shell mktemp -d)

update-pkgbuild-version:
	sed -i 's/^pkgver=.*/pkgver=$(VER_NUM)/' arch/PKGBUILD
.PHONY: update-pkgbuild-version

update-pkgbuild-checksum:
	sed -i "s/sha256sums=.*/sha256sums=('b63548f45d805c971fa7f6a6eb9c6097ce64ef2720883d8ceec021c425bf1b8a')/" arch/PKGBUILD
.PHONY: update-pkgbuild-checksum

CHECKSUM = $(shell curl -sL "https://github.com/uchicago-library/attachment-converter/archive/refs/tags/v$(VER_NUM).tar.gz" | sha256sum | cut -d " " -f 1)
DEBIAN_DATE = $(shell date -R)
FILES_TO_UPDATE = lib/version.ml arch/PKGBUILD debian/changelog ubuntu_wsl/prelude.$(PRELUDE_VER_NUM)/opam ubuntu_wsl/opampack-packs ubuntu_wsl/opampack-upacks

checksum:
	@echo $(CHECKSUM)
.PHONY: checksum

update-ocaml-vernum:
	sed -i 's/let ver_num = \".*\"/let ver_num = \"$(VER_NUM)\"/' lib/version.ml
.PHONY: update-version-dot-ml

update-debian-changelog:
	sed -i "s/attachment-converter (.*) $(DEBIAN_CODENAME)/attachment-converter ($(VER_NUM)-1~$(DEBIAN_CODENAME)) $(DEBIAN_CODENAME)/" debian/changelog
	sed -i "s/[A-Z][a-z][a-z], [0-9][0-9] [A-Z][a-z][a-z] [0-9][0-9][0-9].*/$(DEBIAN_DATE)/" debian/changelog
.PHONY: update-debian-changelog

release-tags:
	git add $(FILES_TO_UPDATE)
	git commit -m "version $(VER_NUM) (testing automation; please ignore this commit)"
# git tag -a v$(VER_NUM) -m 
.PHONY: release-tags

prep-for-release: update-pkgbuild-version update-ocaml-vernum update-debian-changelog prelude opampack

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

opampack-upacks:
	opam show --just-file --field=depends ./attachment-converter.opam | sed '/"ocaml"/s/["{}> =]//g;/"dune"/s/.*/dune/;/{/d' | sed 's/"//g' | paste -sd ' ' > ubuntu_wsl/opampack-upacks
.PHONY: opampack-upacks

# note: this takes a minute or two to run, because it builds attc in a
# fresh sandboxed switch
opampack-packs:
	opam switch remove --yes $(PWD) || true && make sandbox 1> /dev/null && eval $$(opam env) && opam list --short | paste -sd ' ' > ubuntu_wsl/opampack-packs
.PHONY: opampack-packs

opampack: opampack-upacks opampack-packs

prelude:
	scp $(PRELUDE_OPAM_PATH)/opam ubuntu_wsl/prelude.$(PRELUDE_VER_NUM)
.PHONY: prelude

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
