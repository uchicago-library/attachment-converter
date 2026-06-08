#!/bin/bash


# Attachment Converter is distributed under the terms of the GNU
# GPL-3.0-or-later.

# Copyright 2026 Matt Teichman and Nathan Mull.



findOS(){
    if uname | grep -q "Darwin"; then
        if command -v brew > /dev/null; then
            os="brew install"
        else
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            os="brew install"
        fi
    elif uname | grep -q "Linux"; then
        if command -v pacman > /dev/null; then
            os="sudo pacman -S --needed"
        elif command -v apt > /dev/null; then
            os="sudo apt install"
            # if [ $2 = "opam" ]; then
            #     sudo add-apt-repository ppa:avsm/ppa
            #     sudo apt update
        fi
    fi
}

customizeArg(){
    if [ $2 = "vips" ]; then
        if [ "$os" = "sudo pacman -S --needed" ]; then
            customarg="libvips"
        elif [ "$os" = "sudo apt install" ]; then
            customarg="libvips-tools"
        else
            customarg="vips"
        fi
    elif [ $2 = "verapdf" ]; then
        if [ "$os" != "brew install" ]; then
            customarg="NaN"
        else
            customarg="verapdf"
        fi
    elif [ $2 = "catdoc" ]; then
        if [ "$os" != "sudo pacman -S --needed" ]; then
            customarg="NaN"
        else
            customarg="catdoc"
        fi
    else
        customarg=$2
    fi
}

findOS

while [[ $# -gt 0 ]]; do
    customizeArg "$os" $1
    if [ "$customarg" != "NaN" ]; then
        if [ "$os" = "sudo apt install" ]; then
            $os $customarg -y
        else
            $os $customarg
        fi
    else
        $os $customarg
    fi
    shift
done


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
