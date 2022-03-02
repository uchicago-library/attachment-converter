#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

TMP_DIR=$(mktemp -d)

while getopts ":i:o:" opt; do
    case "$opt" in
        i)
            INPUT_EXT=$OPTARG
            ;;

        o)
            OUTPUT_EXT=$OPTARG
            ;;
    esac
done

INPUT="$TMP_DIR/temp-in.$INPUT_EXT"
OUTPUT="$TMP_DIR/temp-out.$OUTPUT_EXT"

cat > $INPUT
vips copy $INPUT $OUTPUT
cat $OUTPUT
