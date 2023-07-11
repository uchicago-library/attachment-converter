#!/bin/bash

if command -v pacman > /dev/null; then
    pacman -S libreoffice pandoc ghostscript gnumeric libvips catdoc
elif command -v brew > /dev/null; then
    brew install libreoffice pandoc ghostscript gnumeric vips verapdf
elif command -v apt > /dev/null; then
    apt install libreoffice pandoc ghostscript gnumeric libvips-tools verapdf
fi
