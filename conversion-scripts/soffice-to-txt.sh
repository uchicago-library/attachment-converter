#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

INPUT=$1
TMP_DIR=$(mktemp -d)
INPUT_NAME=$(basename $INPUT)
OUTPUT="$TMP_DIR/${INPUT_NAME%.*}.txt"

mkdir -p $TMP_DIR/dummy_config/user

{
    soffice --convert-to txt:Text --outdir $TMP_DIR $INPUT
} 1>$TMP_DIR/null 2>&1

cat $OUTPUT
