import glob

from osgeo import gdal

import os

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




tf = tile_files[tf_index]

print("Using tilefile: " + tf)

out_downscaled = os.path.join("data/forestloss/lossyear_downscale",os.path.split(tf)[1])


print("Calculating...")



for i in range(0,20):
    temp_calc = os.path.join("data/forestloss/lossyear_calc", os.path.split(tf)[1] + "_" + str(i) + ".tif")

    if os.path.exists(temp_calc):
        os.remove(temp_calc)

    calc_cmd = "gdal_calc.py " + "--calc=A==" + str(i) " --outfile=" + temp_calc + " -A " + tf + " --co=NBITS=1 --type=Byte --quiet"

    print("Calc command: " + calc_cmd)

    os.system(calc_cmd)


calc_files = glob.glob("data/forestloss/lossyear_calc/" + os.path.split(tf)[1] + "*")


temp_merged = os.path.join("data/forestloss/lossyear_merged", os.path.split(tf)[1])

if os.path.exists(temp_merged):
    os.remove(temp_merged)

merge_cmd = "gdal_merge.py -separate -o " + temp_merged + " " + " ".join(calc_files)

os.system(merge_cmd)

print("Warping...")




warp_opts = "-r average -multi -ot Float32 -ts " + str(target_tile_size) + " " + str(target_tile_size)

warp_cmd = "gdalwarp " + warp_opts + " " + temp_merged + " " + out_downscaled

print("Warp command: " + warp_cmd)

os.system(temp_merged)

print("Done.")