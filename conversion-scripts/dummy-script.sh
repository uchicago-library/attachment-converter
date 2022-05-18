#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

TMP_DIR=$(mktemp -d)
INPUT="$TMP_DIR/temp-in.pdf"

cat > $INPUT
echo "This is nothing"
