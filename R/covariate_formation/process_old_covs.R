
library(raster)

library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")
old_covs <- brick("data/clean/raster/SEAsia_covs")

covs_transfer_names <- c("urban_access",
                         "Pf_temp",
                         "fascicularis",
                         "nemestrina",
                         "leucosphrus_group",
                         "SRTM_elevation")

remove_layers <- setdiff(names(old_covs),covs_transfer_names)

transferred_brick <- dropLayer(old_covs, remove_layers)

writeRaster(transferred_brick,
            "data/raw/covariate_production/nontemporal_final/old_covs",
            format="raster")
