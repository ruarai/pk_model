import glob
import os

import sys


tmp_vrt = "/var/local/tmp/mosaic.vrt"

os.system("gdalbuildvrt " + tmp_vrt + " data/forestloss/lossyear_downscale/*.tif")


os.system("gdal_translate -co 'COMPRESS=DEFLATE' -co 'TILED=YES' mosaic.vrt data/forestloss/full_forestloss.tif")
