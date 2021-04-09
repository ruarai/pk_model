import glob

from osgeo.utils import gdal_calc
from osgeo import gdal

import tempfile
from shutil import copyfile

import sys

import math

tf_index = int(sys.argv[1])-1

print("Starting run for index " + str(tf_index))


tile_files = glob.glob("data/forestloss/lossyear_tiles/*")



curr_pixel_size = 0.00025
target_pixel_size = 0.04166664

scale_factor = target_pixel_size / curr_pixel_size

rds = gdal.Open(tile_files[1])
curr_tile_size = rds.RasterYSize
target_tile_size = rds.RasterYSize / scale_factor

rds = None


warp_opts = gdal.WarpOptions(resampleAlg  = "average",
                            multithread = True,
                            width=target_tile_size, height = target_tile_size,
                            outputType = gdal.GDT_Float32,
                            creationOptions = ["COMPRESS=DEFLATE"])



calc_strings = ["A == " + str(s) for s in range(0,1)]

tf = tile_files[tf_index]

print("Using tilefile: " + tf)

temp_calc = os.path.join("lossyear_calc", os.path.split(tf)[1])
out_downscaled = os.path.join("lossyear_downscale",os.path.split(tf)[1])

if os.path.exists(temp_calc):
    os.remove(temp_calc)

print("Calculating...")



# Create a calculated raster, where each pixel is 1/0 for what == our years of interest (banded by year)
gdal_calc.Calc(calc = calc_strings,
                outfile= temp_calc,
                A = tf,
                creation_options = ["NBITS=1", "COMPRESS=DEFLATE"],
                type = "Byte",
                quiet=True)

print("Warping...")

# Warp it onto our desired scale, using warp_opts
gdal.Warp(out_downscaled,
            temp_calc,
            options = warp_opts)

os.remove(temp_calc)

print("Done.")