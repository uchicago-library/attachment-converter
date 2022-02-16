#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

INPUT=$1
TMP_DIR=$(mktemp -d)
OUTPUT="$TMP_DIR/temp.tif"

vips copy $INPUT $OUTPUT

cat $OUTPUT
