Testing jpeg to tiff conversion
  $ ../../conversion-scripts/vips-wrapper.sh -i jpeg -o tif < test-jpeg.jpeg > converted-test-jpeg-to-tif.tif



Output:
  $ fileType=$(file --mime-type converted-test-jpeg-to-tif.tif | sed -E 's/(.*)(: )(.*)/\3/') 
