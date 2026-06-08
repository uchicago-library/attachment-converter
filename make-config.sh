#!/bin/bash


# Attachment Converter is distributed under the terms of the GNU
# GPL-3.0-or-later.

# Copyright 2026 Matt Teichman and Nathan Mull.


echo %source_type application/pdf
echo %target_type application/pdf
echo %shell_command $HOME/.config/attachment-converter/scripts/soffice-wrapper.sh -i pdf -o pdf
echo %id soffice-pdf-to-pdfa
echo
echo %source_type application/pdf
echo %target_type text/plain
echo %shell_command $HOME/.config/attachment-converter/scripts/pdftotext-wrapper.sh
echo %id pdftotext-pdf-to-text
echo
echo %source_type application/msword
echo %target_type application/pdf
echo %shell_command $HOME/.config/attachment-converter/scripts/soffice-wrapper.sh -i doc -o pdf
echo %id soffice-doc-to-pdfa
echo
echo %source_type application/msword
echo %target_type text/plain
echo %shell_command $HOME/.config/attachment-converter/scripts/soffice-wrapper.sh -i doc -o txt
echo %id soffice-doc-to-txt
echo
echo %source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
echo %target_type application/pdf
echo %shell_command $HOME/.config/attachment-converter/scripts/soffice-wrapper.sh -i docx -o pdf
echo %id soffice-doc-to-pdfa
echo
echo %source_type application/vnd.openxmlformats-officedocument.wordprocessingml.document
echo %target_type text/plain
echo %shell_command $HOME/.config/attachment-converter/scripts/pandoc-wrapper.sh -i docx -o txt
echo %id soffice-doc-to-txt
echo
echo %source_type application/vnd.ms-excel
echo %target_type text/tab-separated-values
echo %shell_command $HOME/.config/attachment-converter/scripts/ssconvert-wrapper.sh -i xls -o tsv
echo %id ssconvert-xls-to-tsv
echo
echo %source_type application/vnd.ms-excel
echo %target_type text/tab-separated-values
echo %shell_command $HOME/.config/attachment-converter/scripts/ssconvert-wrapper.sh -i xlsx -o tsv
echo %id ssconvert-xlsx-to-tsv
echo
echo %source_type image/gif
echo %target_type image/tiff
echo %shell_command $HOME/.config/attachment-converter/scripts/vips-wrapper.sh -i gif -o tif
echo %id vips-gif-to-tif
echo
echo %source_type image/bmp
echo %target_type image/tiff
echo %shell_command $HOME/.config/attachment-converter/scripts/vips-wrapper.sh -i bmp -o tif
echo %id vips-bmp-to-tif
echo
echo %source_type image/jpeg
echo %target_type image/tiff
echo %shell_command $HOME/.config/attachment-converter/scripts/vips-wrapper.sh -i jpeg -o tif
echo %id vips-jpg-to-tif


# This file is part of Attachment Converter.

# Attachment Converter is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# Attachment Converter is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Attachment Converter. If not, see
# <https://www.gnu.org/licenses/>.
