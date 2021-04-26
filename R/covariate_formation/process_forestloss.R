
library(raster)
library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


lossyear_raster <- brick("data/raw/covariate_production/global_forest_change/lossyear_downscale.tif")


blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1


lossyear_raster <- lossyear_raster * blank

names(lossyear_raster) <- c("forestloss_noloss", str_c("forestloss_", 2000+1:19), "forestloss_blank")

lossyear_raster <- dropLayer(lossyear_raster, c("forestloss_noloss",
                                                "forestloss_blank")) # unused layers

writeRaster(lossyear_raster,
            "data/raw/covariate_production/temporal_final/lossyear_stack",
            method = "raster",
            overwrite=TRUE)



treecover_raster <-  brick("data/raw/covariate_production/global_forest_change/treecover_downscale.tif")

treecover_raster <- treecover_raster * blank

names(treecover_raster) <- c(str_c("treecover_", 2000+0:19), "treecover_blank")

treecover_raster <- dropLayer(treecover_raster, c("treecover_blank"))

writeRaster(treecover_raster,
            "data/raw/covariate_production/temporal_final/treecover_stack",
            method = "raster",
            overwrite=TRUE)

