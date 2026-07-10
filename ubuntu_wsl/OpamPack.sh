#!/bin/bash


# Attachment Converter is distributed under the terms of the GNU
# GPL-3.0-or-later.

# Copyright 2026 Matt Teichman and Nathan Mull.


# Initialization

USER_PACKAGES=$(cat ./opampack-upacks)

git clone https://github.com/ocaml/opam-repository.git --depth=1



# Create an empty directory that will contain the opam files

mkdir opamroot



# Set it as the default OPAM root directory

export OPAMROOT=$PWD/opamroot



# Initialize it as an Opam repository, without creating a switch to
# prevent the installation of an OCaml compiler.

echo n \
    | opam init \
             --bare \
             --disable-sandboxing \
             --disable-shell-hook \
             --root=$PWD/opamroot \
             $PWD/opam-repository
eval $(opam env)



# Create an empty switch named =opampack=

opam switch create --empty opampack
eval $(opam env --switch=opampack)



# Find the complete list of necessary packages

PACKAGES=$(cat ./opampack-packs)

# In the repository, keep only the required packages:

cd opam-repository
mv packages packages_old
mkdir packages
for p in $PACKAGES ; do
  mv packages_old/$p packages
done

rm -r packages/prelude/*
cp -r ../prelude.100.7 packages/prelude/

# Remove unnecessary packages and git files

rm -rf packages_old
rm -rf .git
cd ..

rm -rf opamroot
mkdir opamroot



# Set it as the default OPAM root directory

export OPAMROOT=$PWD/opamroot



# Initialize it as an Opam repository, without creating a switch to
# prevent the installation of an OCaml compiler.

echo n \
    | opam init \
             --bare \
             --disable-sandboxing \
             --disable-shell-hook \
             --root=$PWD/opamroot \
             $PWD/opam-repository
eval $(opam env)



# Create an empty switch named =opampack=

opam switch create --empty opampack
eval $(opam env --switch=opampack)

# Download all the required packages for the installation

opam install -y --download-only $USER_PACKAGES



# Create a script to extract the =tar.gz= file and to install the packages.

INSTALL_SCRIPT=install.sh

cat << EOF > $INSTALL_SCRIPT
#!/bin/bash
export OPAMROOT=\$PWD/ubuntu_wsl/opamroot
eval \$(opam env --root=\$PWD/ubuntu_wsl/opamroot)
opam install -y --assume-depexts $USER_PACKAGES
EOF
chmod +x $INSTALL_SCRIPT



# Make a =tar.gz= of all the needed files for exporting OPAM

cd ..  # back in _build
tar -zcf opampack.tar.gz ubuntu_wsl
rm -rf ubuntu_wsl


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
