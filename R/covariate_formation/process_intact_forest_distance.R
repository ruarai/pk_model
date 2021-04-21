library(raster)

library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")
old_covs <- brick("data/clean/raster/SEAsia_covs")

forest_intact <- old_covs$forest_intact


prox_tmp <- tempfile()
in_tmp <- tempfile()

writeRaster(forest_intact, in_tmp, method="GTiff")


prox_cmd <- glue::glue("gdal_proximity.py -srcband 1 -distunits GEO -values 1 -maxdist 3.0 -ot Float32 -of GTiff {in_tmp} {prox_tmp}")

system(prox_cmd)
