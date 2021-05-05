
library(raster)

library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")
old_covs <- brick("data/clean/raster/SEAsia_covs")

covs_transfer_names <- c("SRTM_elevation",
                         "nemestrina",
                         "leucosphyrus_group",
                         "fascicularis",
                         "Pf_temp",
                         "TCW_SD",
                         "TCW_mean",
                         "TCB_SD",
                         "forest_disturbed",
                         "forest_intact",
                         "human_pop")

remove_layers <- setdiff(names(old_covs),covs_transfer_names)

transferred_brick <- dropLayer(old_covs, remove_layers)

writeRaster(transferred_brick,
            "data/raw/covariate_production/nontemporal_final/old_covs",
            format="raster",
            overwrite = TRUE)



# 
# 
# old_covs_temporal <- brick("data/clean/raster/mbs_raster_temporal")
# 
# take_old_temporals <- c("fascicularis",
#                         "nemestrina",
#                         "leucosphyrus_group",
#                         "forest_intact",
#                         "forest_disturbed")
# 
# detect_any <- function(a, b){
#   any(sapply(b, function(i) str_detect(a, i)))
# }
# 
# keep_layers <- names(old_covs_temporal)[sapply(names(old_covs_temporal), function(x) detect_any(x, take_old_temporals))]
# 
# 
# remove_layers <- setdiff(names(old_covs_temporal), keep_layers)
# 
# transferred_brick <- dropLayer(old_covs_temporal, remove_layers)
# 
# transferred_brick <- extend(transferred_brick, extent(old_covs))
# 
# blank <- raster("data/clean/raster/SEAsia_extent")
# blank[!is.na(blank)] <- 1
# 
# temporal_names <- names(transferred_brick)
# 
# transferred_brick[is.na(transferred_brick)] <- 0
# transferred_brick <- transferred_brick * blank
# 
# names(transferred_brick) <- temporal_names
# 
# writeRaster(transferred_brick,
#             "data/raw/covariate_production/temporal_final/old_covs",
#             format="raster",
#             overwrite = TRUE)

