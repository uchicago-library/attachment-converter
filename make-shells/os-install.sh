#!/bin/bash

if uname | grep -q "Darwin"; then
    if command -v brew > /dev/null; then
        brew install $1
    else
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        brew install $1
    fi
elif uname | grep -q "Linux"; then
    if command -v pacman > /dev/null; then
        sudo pacman -S $1
    elif command -v apt > /dev/null; then
        sudo apt-get update
        sudo apt install $1
    fi
fi
