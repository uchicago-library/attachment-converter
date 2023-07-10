#!/bin/bash

# if command -v pacman > /dev/null; then
#     echo windows
#     echo not mac and not linux
# elif command -v brew > /dev/null; then
#     echo mac
#     echo not windows and not linux
# elif command -v apt > /dev/null; then
#     echo linux
#     echo not windows and not mac
# fi

uname &> /dev/null
if [ $? == 'Darwin' ]; then
    if command -v brew > /dev/null; then
        echo brew is installed
    else
        echo please install brew
    fi
elif command -v pacman > /dev/null; then
    echo pacman is installed
elif command -v apt > /dev/null; then
    echo apt is installed
fi