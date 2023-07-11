Testing pdf to pdfa conversion:
  $ ../../conversion-scripts/soffice-wrapper.sh -i pdf -o pdf < test-pdf.pdf > converted-test-pdf-to-pdfa.pdf
  $ if [ $? -eq 0 ]; then echo "pdf to pdfa conversion was successfull"; else "pdf to pdfa conversion failed"; fi;
  pdf to pdfa conversion was successfull
  $ pdftotext expected-converted-test-pdf-to-pdfa.pdf
  $ pdftotext converted-test-pdf-to-pdfa.pdf
  $ diff expected-converted-test-pdf-to-pdfa.txt converted-test-pdf-to-pdfa.txt
  $ fileType=$(file --mime-type converted-test-pdf-to-pdfa.pdf | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "application/pdf" ]; then echo "pdf to pdfa file validation successfull"; else echo "pdf to pdfa file validation failed.\nExpected file type: application/pdf. Got file type: $fileType"; fi;
  pdf to pdfa file validation successfull

Testing doc to pdfa conversion:
  $ ../../conversion-scripts/soffice-wrapper.sh -i doc -o pdf < test-doc.doc > converted-test-doc-to-pdfa.pdf
  $ if [ $? -eq 0 ]; then echo "doc to pdfa conversion was successfull"; else "doc to pdfa conversion failed"; fi;
  doc to pdfa conversion was successfull
  $ pdftotext expected-converted-test-doc-to-pdfa.pdf
  $ pdftotext converted-test-doc-to-pdfa.pdf
  $ diff expected-converted-test-doc-to-pdfa.txt converted-test-doc-to-pdfa.txt
  $ fileType=$(file --mime-type converted-test-doc-to-pdfa.pdf | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "application/pdf" ]; then echo "doc to pdfa file validation successfull"; else echo "doc to pdfa file validation failed.\nExpected file type: application/pdf. Got file type: $fileType"; fi;
  doc to pdfa file validation successfull

Testing doc to text conversion:
  $ ../../conversion-scripts/soffice-wrapper.sh -i doc -o txt < test-doc.doc > converted-test-doc-to-text.txt
  $ if [ $? -eq 0 ]; then echo "doc to txt conversion was successfull"; else "doc to txt conversion failed"; fi;
  doc to txt conversion was successfull
  $ diff expected-converted-test-doc-to-text.txt converted-test-doc-to-text.txt
  $ fileType=$(file --mime-type converted-test-doc-to-text.txt | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "text/plain" ]; then echo "doc to text file validation successfull"; else echo "doc to text file validation failed.\nExpected file type: text/plain. Got file type: $fileType"; fi;
  doc to text file validation successfull

Testing docx to pdfa conversion
  $ ../../conversion-scripts/soffice-wrapper.sh -i docx -o pdf < test-docx.docx > converted-test-docx-to-pdf.pdf
  $ if [ $? -eq 0 ]; then echo "docx to pdfa conversion was successfull"; else "docx to pdfa conversion failed"; fi;
  docx to pdfa conversion was successfull
  $ pdftotext expected-converted-test-docx-to-pdf.pdf
  $ pdftotext converted-test-docx-to-pdf.pdf
  $ diff expected-converted-test-docx-to-pdf.txt converted-test-docx-to-pdf.txt
  $ fileType=$(file --mime-type converted-test-docx-to-pdf.pdf | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "application/pdf" ]; then echo "docx to pdfa file validation successfull"; else echo "docx to pdfa file validation failed.\nExpected file type: application/pdf. Got file type: $fileType"; fi;
  docx to pdfa file validation successfull

Testing docx to text conversion
  $ ../../conversion-scripts/pandoc-wrapper.sh -i docx -o txt < test-docx.docx > converted-test-docx-to-text.txt
  $ if [ $? -eq 0 ]; then echo "docx to txt conversion was successfull"; else "docx to txt conversion failed"; fi;
  docx to txt conversion was successfull
  $ diff expected-converted-test-docx-to-text.txt converted-test-docx-to-text.txt
  $ fileType=$(file --mime-type converted-test-docx-to-text.txt | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "text/plain" ]; then echo "docx to text file validation successfull"; else echo "docx to text file validation failed.\nExpected file type: text/plain. Got file type: $fileType"; fi;
  docx to text file validation successfull

Testing xls to tsv conversion
  $ ../../conversion-scripts/soffice-wrapper.sh -i xls -o tsv < test-xls.xls > converted-test-xls-to-tsv.tsv
  $ if [ $? -eq 0 ]; then echo "xls to tsv conversion was successfull"; else "xls to tsv conversion failed"; fi;
  xls to tsv conversion was successfull
  $ diff expected-converted-test-xls-to-tsv.tsv converted-test-xls-to-tsv.tsv
  $ validTsv=$(awk -F'\t' '{print NF}' converted-test-xls-to-tsv.tsv | sort -nu | wc -l | awk '{gsub(/^ +| +$/,"")} {print "" $0 ""}')
  $ if [ $validTsv -eq 1 ]; then echo "xls to tsv file validation successfull"; else echo "xls to tsv file validation failed. All rows don't have the same number of columns"; fi;
  xls to tsv file validation successfull

Testing gif to tiff conversion
  $ ../../conversion-scripts/vips-wrapper.sh -i gif -o tif < test-gif.gif > converted-test-gif-to-tif.tif
  $ if [ $? -eq 0 ]; then echo "gif to tiff conversion was successfull"; else "gif to tiff conversion failed"; fi;
  gif to tiff conversion was successfull
  $ fileType=$(file --mime-type converted-test-gif-to-tif.tif | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "image/tiff" ]; then echo "gif to tiff file validation successfull"; else echo "gif to tiff file validation failed.\nExpected file type: image/tiff Got file type: $fileType"; fi;
  gif to tiff file validation successfull

Testing bmp to tiff conversion
  $ ../../conversion-scripts/vips-wrapper.sh -i bmp -o tif < test-bmp.bmp > converted-test-bmp-to-tif.tif
  $ if [ $? -eq 0 ]; then echo "bmp to tiff conversion was successfull"; else "bmp to tiff conversion failed"; fi;
  bmp to tiff conversion was successfull
  $ fileType=$(file --mime-type converted-test-bmp-to-tif.tif | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "image/tiff" ]; then echo "bmp to tiff file validation successfull"; else echo "bmp to tiff file validation failed.\nExpected file type: image/tiff. Got file type: $fileType"; fi;
  bmp to tiff file validation successfull

Testing jpeg to tiff conversion
  $ ../../conversion-scripts/vips-wrapper.sh -i jpeg -o tif < test-jpeg.jpeg > converted-test-jpeg-to-tif.tif
  $ if [ $? -eq 0 ]; then echo "jpeg to tiff conversion was successfull"; else "jpeg to tif conversion failed"; fi;
  jpeg to tiff conversion was successfull
  $ fileType=$(file --mime-type converted-test-jpeg-to-tif.tif | sed -E 's/(.*)(: )(.*)/\3/') 
  $ if [ $fileType == "image/tiff" ]; then echo "jpeg to tiff file validation successfull"; else echo "jpeg to tiff file validation failed.\nExpected file type: image/tiff. Got file type: $fileType" ; fi;
  jpeg to tiff file validation successfull
