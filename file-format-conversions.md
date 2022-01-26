# File Format Conversions

This is a reference document for the conversions we would like to consider for testing. The utilities covered in this document are:

* LibreOffice
* PDF2ARCHIVE
* Pandoc
* ImageMagick
* libvips
* pdftotext

For now we're interested in the following conversions.

| Source  |  Target   | Utilities                         |
| :-----: | :-------: | :-------------------------------- |
| `.pdf`  | PDF-A 1b  | LibreOffice, PDF2ARCHIVE          |
| `.pdf`  | plaintext | pdftotext                         |
| `.doc`  | PDF-A 1b  | LibreOffice                       |
| `.doc`  | plaintext | LibreOffice                       |
| `.docx` | PDF-A 1b  | LibreOffice, Pandoc → PDF2ARCHIVE |
| `.docx` | plaintext | LibreOffice, Pandoc               |
| `.xls`  |    TSV    | LibreOffice                       |
| `.xlsx` |    TSV    | LibreOffice                       |
| `.gif`  |   TIFF    | LibreOffice, ImageMagick, libvips |
| `.bmp`  |   TIFF    | LibreOffice, ImageMagick, libvips |
| `.jpg`  |   TIFF    | LibreOffice, ImageMagick, libvips |



## LibreOffice

### {`.doc`, `.docx`, `.pdf`} → PDF-A 1b

```
$ soffice -env:UserInstallation=file:///ABSOLUTE/PATH/TO/CONFIG --convert-to pdf:writer_pdf_Export --outdir OUTPUT_DIR INPUT_FILE
```

where `ABSOLUTE/PATH/TO/CONFIG/user/registrymodifications.xcu` contains the following line.

```
<item oor:path="/org.openoffice.Office.Common/Filter/PDF/Export"><prop oor:name="SelectPdfVersion" oor:op="fuse"><value>1</value></prop></item>
```

#### Notes

* The user profile used for the `UserInstallation` bootstrap variable makes LibreOffice convert to PDF-A instead of vanilla PDF. It doesn't need to complete; given an incomplete user profile, LibreOffice will fill in the missing data.
* If any intermediate directories in the path `OUTPUT_DIR` do not exist, `soffice` will create them.
* You cannot specify an output file name, only its directory. If no output directory is specified `.` is used. So the output directory should be specified to be something other than `.` in the case of converting PDFs if you don't want to lose the original version. *For all following LibreOffice conversions, the `--outdir` option is left out.*
* In theory, we believe `soffice --convert-to pdf:writer_pdf_Export:SelectPdfVersion=1 INPUT_FILE` should do the same thing as above, but it doesn't seem to work, the output files are not PDF-A compliant.
* The output for a `.pdf` file will occasionally be noncompliant according to `verapdf	`, but in the few cases we've checked, the output is compliant according to the veraPDF REST service. **Note: These two tools do not always agree on compliance.**
* This should, in principle, also be possible for PDF-A 2b and 3b, but we have not determined how to do this.



### {`.doc`, `.docx`} → `.txt`

```
$ soffice --convert-to txt:Text INPUT_FILE
```

#### Notes

* This does not work for `.pdf` files. The command `soffice --infilter="writer_pdf_import" --convert-to txt:Text INTPUT_FILE` runs but creates an empty plaintext file, even for very simple PDFs. The pipeline `.pdf` → `.doc` → `.txt` has the same behavior.



### {`.xls`, `.xlsx`} → `.tsv`

```
$ soffice --convert-to tsv:"Text - txt - csv (StarCalc)":9,34,0 INPUT_FILE
```

#### Notes

* See [this page](https://wiki.openoffice.org/wiki/Documentation/DevGuide/Spreadsheets/Filter_Options#Filter_Options_for_the_CSV_Filter) for details on the filter options (`9` here is the ASCII value for horizontal tab).



### {`.gif`, `.bmp`, `.jpg`} → `.tif`

```
$ soffice --convert-to tif INPUT_FILE
```



## PDF2ARCHIVE

### `.pdf` → PDF-A 1b

```
 $ ./pdf2archive --validate INPUT_FILE OUTPUT_FILE_NAME
```

#### Notes

* All directories in the path `OUTPUT_FILE_NAME` must exist; `pdf2Archive` does not create directories.
* The input file is expected to be in the same place as the script; in the least, the script seems to want its own local copy of `verapdf` for validation. It may be very simple to fork the project and fix this.
* There are files which LibreOffice failed to make compliant that `pdf2Archive` handled, and vice versa. **It may be worth using both and checking if either succeeds against `verapdf`.**



## Pandoc

### `.docx` → {`.pdf`, `.txt`}

```
$ pandoc INPUT_FILE -o OUTPUT_FILE_NAME
```

#### Notes

* Pandoc determines the which conversion to used based on the `INPUT_FILE` and the extension of `OUTPUT_FILE_NAME`.
* These seem to be the only relevant conversions for Pandoc ([This page](https://pandoc.org/MANUAL.html#options) has all possible input/output file types). With this, we have the pipeline `.docx` → `.pdf` → PDF-A 1b.
* All directories in the path `OUTPUT_FILE_NAME` must exist; Pandoc does not create directories.



## ImageMagick

### {`.gif`, `.bmp`, `.jpg`} → `.tif`
```
$ convert INPUT_FILE OUTPUT_FILE_NAME
```

#### Notes

* Like Pandoc, ImageMagick performs implicit file conversion.
* All directories in the path `OUTPUT_FILE_NAME` must exist; `convert` does not create directories.



## libvips

### {`.gif`, `.bmp`, `.jpg`} → `.tif`

```
$ vips copy INPUT_FILE OUTPUT_FILE_NAME
```

#### Notes

* Like Pandoc, libvips performs implicit file conversion.
* All directories in the path `OUTPUT_FILE_NAME` must exist; libvips does not create directories.



## pdftotext

### `.pdf` → `.txt`

```
$ pdftotext INPUT_FILE OUTPUT_FILE_NAME
```

#### Notes

* All directories in the path `OUTPUT_FILE_NAME` must exist; libvips does not create directories.
* It seems to be included by default in many Linux distros, but is available for Windows in Xpdf.
* The output file is not great, especially if the pdf is complex (e.g., has a lot of mathematical symbols or tables) but none of the other tools handle this conversion.
