

library(raster)

setwd("/data/gpfs/projects/punim1449/knowlesi_ruarai")

full_pred_matrix <- readRDS(file = "output/update/full_mbs_human.Rds")

point_means <- colMeans(full_pred_matrix)




mbs_land <- raster('data/clean/raster/mbs_mask.grd')
blank_raster <- mbs_land - 1


mean_raster <- setValues(blank_raster, point_means)
plot(mean_raster)

library(proxyC)

point_stdevs <- colSds(full_pred_matrix)


stdev_raster <- setValues(blank_raster, point_stdevs)
plot(stdev_raster)
