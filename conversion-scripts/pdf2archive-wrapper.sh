#!/bin/bash

trap 'rm -rf temp-in.pdf temp-out.pdf' EXIT

cd pdf2archive
cat > temp-in.pdf
./pdf2archive temp-in.pdf temp-out.pdf
cat temp-out.pdf
