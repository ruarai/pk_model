
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

new_cov_files <- list.files("data/raw/covariate_production/gee_covs/", pattern="*.tif",
                            full.names = TRUE)

new_cov_bandnames <- list.files("data/raw/covariate_production/gee_covs/", pattern="*.tif") %>%
  str_replace(".tif","")

library(raster)

blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1


rasters <- lapply(new_cov_files, raster)

resampled <- lapply(rasters, function(x) resample(x, blank))

new_stack <- stack(resampled)

new_stack <- new_stack * blank

plot(new_stack)

names(new_stack) <- new_cov_bandnames


writeRaster(new_stack, 
            "data/raw/covariate_production/nontemporal_final/google_earth_engine", 
            format="raster",
            overwrite=TRUE)
