#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

INPUT=$1
TMP_DIR=$(mktemp -d)
INPUT_NAME=$(basename $INPUT)
OUTPUT="$TMP_DIR/${INPUT_NAME%.*}.tsv"

mkdir -p $TMP_DIR/dummy_config/user

{
    soffice --convert-to tsv:"Text - txt - csv (StarCalc)":9,34,0 --outdir $TMP_DIR $INPUT
} 1>$TMP_DIR/null 2>&1

cat $OUTPUT
