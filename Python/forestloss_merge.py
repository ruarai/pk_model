import glob
import os

import sys


merge_dir_in = sys.argv[1]
merge_file_out = sys.argv[2]


tmp_vrt = "/var/local/tmp/mosaic.vrt"

os.system(f"gdalbuildvrt {tmp_vrt} {merge_dir_in}/*.tif")

os.system(f"gdal_translate -co 'COMPRESS=DEFLATE' -co 'TILED=YES' {tmp_vrt} data/forestloss/{merge_file_out}")
