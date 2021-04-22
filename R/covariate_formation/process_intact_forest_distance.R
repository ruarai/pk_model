library(raster)

library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


# Produced by gdal_distance in QGIS
# I was unable to make this automated in R!!
forest_intact_dist <- raster("data/raw/covariate_production/intact_forest_distance/intact_forest_distance.tif")



blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1

forest_intact_dist <- forest_intact_dist * blank
forest_intact_dist[forest_intact_dist > 1] <- 1 # Max 100km distance to consider

forest_intact_dist_stack <- stack(forest_intact_dist)

names(forest_intact_dist_stack) <- "intact_forest_proximity"

writeRaster(forest_intact_dist_stack,
            "data/raw/covariate_production/nontemporal_final/intact_forest_proximity",
            method = "raster",
            overwrite = TRUE)
