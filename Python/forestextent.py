import glob

from osgeo import gdal

import os

import tempfile
from shutil import copyfile

import sys

import math

tf_index = int(sys.argv[1])-1

print("Starting run for index " + str(tf_index))


lossyear_tile_files = sorted(glob.glob("data/forestloss/lossyear_tiles/*"))
treecover2000_tile_files = sorted(glob.glob("data/forestloss/treecover2000_tiles/*"))



curr_pixel_size = 0.00025
target_pixel_size = 0.04166664

scale_factor = target_pixel_size / curr_pixel_size

rds = gdal.Open(lossyear_tile_files[1])
curr_tile_size = rds.RasterYSize
target_tile_size = rds.RasterYSize / scale_factor

rds = None



loss_tf = lossyear_tile_files[tf_index]
treecover2000_tf = treecover2000_tile_files[tf_index]

print("Using loss tilefile: " + loss_tf)
print("Using treecover2000 tilefile: " + treecover2000_tf)


path_tmp_calc = "data/forestloss/temp_treecover2000"

calc_files = glob.glob(path_tmp_calc + "/" + os.path.split(tf)[1] + "*")
for calc_file in calc_files:
    os.remove(calc_file)




print("Calculating...")


for i in range(0, 21):
    temp_calc = os.path.join(path_tmp_calc, os.path.split(tf)[1] + "_" + str(i).rjust(3,'0') + ".tif")

    if os.path.exists(temp_calc):
        os.remove(temp_calc)

    calc_opts = " --co=NBITS=1 --type=Byte --quiet"
    calc_string = f"=((A>{i}) or (A==0)) and (B>=0.5)"

    calc_cmd = f"gdal_calc.py --calc={calc_string} --outfile={temp_calc} -A {loss_tf} -B {treecover2000_tf} {calc_opts}"

    print("Calc command: " + calc_cmd)

    os.system(calc_cmd)


calc_files = sorted(glob.glob(path_tmp_calc + "/" + os.path.split(tf)[1] + "*"))


temp_merged = os.path.join(path_tmp_calc, "merged_" + os.path.split(tf)[1])

if os.path.exists(temp_merged):
    os.remove(temp_merged)

print("Merging...")

tmp_vrt = f"data/forestloss/temp/{tf_index}.vrt"

merge_cmd = f"gdalbuildvrt -separate {tmp_vrt} { " ".join(calc_files) }"

print("Merge command: " + merge_cmd)

os.system(merge_cmd)

trans_cmd = f"gdal_translate {tmp_vrt} {temp_merged}"

print(trans_cmd)

os.system(trans_cmd)

for calc_file in calc_files:
    os.remove(calc_file)

os.remove(tmp_vrt)

print("Warping...")



out_downscaled = os.path.join("data/forestloss/treecover_downscale", os.path.split(tf)[1])

warp_opts = f"-multi -wo NUM_THREADS=ALL_CPUS -r average -ot Float32 -ts {target_tile_size} {target_tile_size}"

warp_cmd = f"gdalwarp --config GDAL_CACHEMAX 500 {warp_opts} {temp_merged} {out_downscaled}"

print("Warp command: " + warp_cmd)

os.system(warp_cmd)

os.remove(temp_merged)

print("Done.")