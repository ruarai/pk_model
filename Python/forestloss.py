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



tf = tile_files[tf_index]

print("Using tilefile: " + tf)


path_tmp_calc = "/var/local/tmp"

calc_files = glob.glob(path_tmp_calc + "/" + os.path.split(tf)[1] + "*")
for calc_file in calc_files:
    os.remove(calc_file)




print("Calculating...")


for i in range(0, 20):
    temp_calc = os.path.join(path_tmp_calc, os.path.split(tf)[1] + "_" + str(i).rjust(3,'0') + ".tif")

    if os.path.exists(temp_calc):
        os.remove(temp_calc)

    calc_cmd = "gdal_calc.py " + "--calc=A==" + str(i) + " --outfile=" + temp_calc + " -A " + tf + " --co=NBITS=1 --type=Byte --quiet"

    print("Calc command: " + calc_cmd)

    os.system(calc_cmd)


calc_files = sorted(glob.glob(path_tmp_calc + "/" + os.path.split(tf)[1] + "*"))


temp_merged = os.path.join(path_tmp_calc, "merged_" + os.path.split(tf)[1])

if os.path.exists(temp_merged):
    os.remove(temp_merged)

print("Merging...")

tmp_vrt = "/var/local/tmp/" + str(tf_index) + ".vrt"

merge_cmd = "gdalbuildvrt -separate " + tmp_vrt + " " + " ".join(calc_files)

print("Merge command: " + merge_cmd)

os.system(merge_cmd)

trans_cmd = "gdal_translate " + tmp_vrt + " " + temp_merged

print(trans_cmd)

os.system(trans_cmd)

for calc_file in calc_files:
    os.remove(calc_file)

print("Warping...")



out_downscaled = os.path.join("data/forestloss/lossyear_downscale",os.path.split(tf)[1])

warp_opts = "-r average -multi -ot Float32 -multi  -ts " + str(target_tile_size) + " " + str(target_tile_size)

warp_cmd = "gdalwarp --config GDAL_CACHEMAX 500 " + warp_opts + " " + temp_merged + " " + out_downscaled

print("Warp command: " + warp_cmd)

os.system(warp_cmd)

os.remove(temp_merged)

print("Done.")