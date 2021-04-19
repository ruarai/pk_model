
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

new_cov_files <- list.files("data/raw/covariate_production/new_covs/", pattern="*.tif",
                            full.names = TRUE)

library(raster)

blank <- raster("data/clean/raster/SEAsia_extent")


rasters <- lapply(new_cov_files, raster)

resampled <- lapply(rasters, function(x) resample(x, blank))

new_stack <- stack(resampled)

plot(new_stack)

old_stack <- brick("data/clean/raster/SEAsia_covs")


full_stack <- stack(new_stack, old_stack)

writeRaster(full_stack, "data/clean/raster/SEAsia_covs_v2",
            format = "raster")


mbs_mask <- raster("data/clean/raster/mbs_mask")

mbs_stack <- crop(full_stack,mbs_mask)

writeRaster(mbs_stack, "data/clean/raster/covs_current_v2.grd", format="raster")

writeRaster(mbs_stack, "data/clean/raster/mbs_raster_nontemporal_v2.grd", format="raster")
