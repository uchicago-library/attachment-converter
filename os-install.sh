#!/bin/bash

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
        $os $customarg
    fi
    shift
done
