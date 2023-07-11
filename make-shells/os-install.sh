#!/bin/bash

if uname | grep -q "Darwin"; then
    if command -v brew > /dev/null; then
        echo brew install $1
    else
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        echo brew install $1
    fi
elif uname | grep -q "Linux"; then
    if command -v pacman > /dev/null; then
        echo pacman -S $1
    elif command -v apt > /dev/null; then
        echo apt install $1
    fi
fi
