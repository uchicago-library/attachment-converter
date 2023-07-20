Testing bmp to tiff conversion
  $ ../../conversion-scripts/vips-wrapper.sh -i bmp -o tif < test-bmp.bmp > converted-test-bmp-to-tif.tif



Output:
  $ fileType=$(file --mime-type converted-test-bmp-to-tif.tif | sed -E 's/(.*)(: )(.*)/\3/') 

