#!/bin/bash

trap 'rm -rf "$TMP_DIR"' EXIT

INPUT=$1
TMP_DIR=$(mktemp -d)
INPUT_NAME=$(basename $INPUT)
OUTPUT="$TMP_DIR/${INPUT_NAME%.*}.pdf"

CONFIG='
<?xml version="1.0" encoding="UTF-8"?>
<oor:items xmlns:oor="http://openoffice.org/2001/registry" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<item oor:path="/org.openoffice.Office.Common/Filter/PDF/Export"><prop oor:name="SelectPdfVersion" oor:op="fuse"><value>1</value></prop></item>
</oor:items>
'

mkdir -p $TMP_DIR/dummy_config/user

{
    echo $CONFIG | tee $TMP_DIR/dummy_config/user/registrymodifications.xcu
    soffice -env:UserInstallation=file://$TMP_DIR/dummy_config --convert-to pdf:writer_pdf_Export --outdir $TMP_DIR $INPUT
} 1>$TMP_DIR/null 2>&1

cat $OUTPUT
