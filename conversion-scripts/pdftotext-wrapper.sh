#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

TMP_DIR=$(mktemp -d)
INPUT="$TMP_DIR/temp-in.pdf"
OUTPUT="$TMP_DIR/temp-out.txt"

cat > $INPUT
pdftotext $INPUT $OUTPUT
cat $OUTPUT
