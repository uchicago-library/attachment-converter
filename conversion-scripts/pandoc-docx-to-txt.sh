#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

TMP_DIR=$(mktemp -d)
INPUT="$TMP_DIR/temp-in.docx"
OUTPUT="$TMP_DIR/temp-out.txt"

cat > $INPUT
pandoc $INPUT -o $OUTPUT
cat $OUTPUT
