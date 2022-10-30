library(raster)
library(tidyverse)



source("code/R/figures/maps_common.R")

species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

risk_new <- brick("output/update/out_pred_brick_final.grd")

mask_raster <- function(x) {
  masked <- x * species_extent
  masked[masked == 0] <- NA
  return(masked)
}

risk_old <- brick("data/clean/raster/SEAsia.tif")


risk_var_new <- mask_raster(risk_new$var)

risk_sd_old <- mask_raster(risk_old[[2]])


range(getValues(risk_var_new), na.rm = TRUE)
range(getValues(risk_old[[4]]), na.rm = TRUE)
