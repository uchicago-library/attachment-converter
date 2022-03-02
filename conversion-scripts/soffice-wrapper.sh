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

INPUT="$TMP_DIR/temp.$INPUT_EXT"
mkdir $TMP_DIR/out
OUTPUT="$TMP_DIR/out/temp.$OUTPUT_EXT"

cat > $INPUT

case "$OUTPUT_EXT" in
    pdf)
        CONFIG='
        <?xml version="1.0" encoding="UTF-8"?>
        <oor:items xmlns:oor="http://openoffice.org/2001/registry" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <item oor:path="/org.openoffice.Office.Common/Filter/PDF/Export"><prop oor:name="SelectPdfVersion" oor:op="fuse"><value>1</value></prop></item>
        </oor:items>
        '

        mkdir -p $TMP_DIR/dummy_config/user

        {
            echo $CONFIG | tee $TMP_DIR/dummy_config/user/registrymodifications.xcu
            soffice -env:UserInstallation=file://$TMP_DIR/dummy_config --convert-to pdf:writer_pdf_Export --outdir $TMP_DIR/out $INPUT
        } 1>$TMP_DIR/null 2>&1
        ;;
    txt)
        {
            soffice --convert-to txt:Text --outdir $TMP_DIR/out $INPUT
        } 1>$TMP_DIR/null 2>&1
        ;;
    tsv)
        {
            soffice --convert-to tsv:"Text - txt - csv (StarCalc)":9,34,0 --outdir $TMP_DIR/out $INPUT
        } 1>$TMP_DIR/null 2>&1

       ;;
esac

cat $OUTPUT
