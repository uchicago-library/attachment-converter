#!/bin/bash
echo %source_type application/pdf
echo %target_type application/pdf
echo %shell_command $(pwd)/conversion-scripts/pdf2archive-wrapper.sh
echo %id pdf2archive-pdf-to-pdfa
echo
echo %source_type application/pdf
echo %target_type application/pdf
echo %shell_command $(pwd)/conversion-scripts/soffice-wrapper.sh -i pdf -o pdf
echo %id soffice-pdf-to-pdfa
echo
echo %source_type application/pdf
echo %target_type text/plain
echo %shell_command $(pwd)/conversion-scripts/pdftotext-wrapper.sh
echo %id pdftotext-pdf-to-text
echo
echo %source_type application/msword
echo %target_type application/pdf
echo %shell_command $(pwd)/conversion-scripts/soffice-wrapper.sh -i doc -o pdf
echo %id soffice-doc-to-pdfa
echo
echo %source_type application/msword
echo %target_type text/plain
echo %shell_command $(pwd)/conversion-scripts/soffice-wrapper.sh -i doc -o txt
echo %id soffice-doc-to-txt
echo
echo %source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
echo %target_type application/pdf
echo %shell_command $(pwd)/conversion-scripts/soffice-wrapper.sh -i docx -o pdf
echo %id soffice-doc-to-pdfa
echo
echo %source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
echo %target_type text/plain
echo %shell_command $(pwd)/conversion-scripts/pandoc-wrapper.sh -i docx -o txt
echo %id soffice-doc-to-txt
echo
echo %source_type application/vnd.ms-excel
echo %target_type text/tab-separated-values
echo %shell_command $(pwd)/conversion-scripts/ssconvert-wrapper.sh -i xls -o tsv
echo %id ssconvert-xls-to-tsv
echo
echo %source_type application/vnd.ms-excel
echo %target_type text/tab-separated-values
echo %shell_command $(pwd)/conversion-scripts/ssconvert-wrapper.sh -i xlsx -o tsv
echo %id ssconvert-xlsx-to-tsv
echo
echo %source_type image/gif
echo %target_type image/tiff
echo %shell_command $(pwd)/conversion-scripts/vips-wrapper.sh -i gif -o tif
echo %id vips-gif-to-tif
echo
echo %source_type image/bmp
echo %target_type image/tiff
echo %shell_command $(pwd)/conversion-scripts/vips-wrapper.sh -i bmp -o tif
echo %id vips-bmp-to-tif
echo
echo %source_type image/jpeg
echo %target_type image/tiff
echo %shell_command $(pwd)/conversion-scripts/vips-wrapper.sh -i jpeg -o tif
echo %id vips-jpg-to-tif
