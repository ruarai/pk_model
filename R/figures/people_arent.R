

library(raster)
library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


risk_4c <- raster("output/update/rasters/buildup_4c_risk_mean.tif")

pred_SEA <- brick("data/clean/raster_updated/prediction_SEA")
human_pop <- pred_SEA[["human_pop"]]




risk_vals <- getValues(risk_4c)
pop_vals <- getValues(human_pop)
elevation_vals <- rescale(getValues(pred_SEA[["SRTM_elevation"]]))
tcw_sd_vals <- rescale(getValues(pred_SEA[["TCW_SD"]]))
nemestrina_vals <- rescale(getValues(pred_SEA[["nemestrina"]]))

library(scales)

keep_vals <- !is.na(elevation_vals)

plot(log(pop_vals[keep_vals]),risk_vals[keep_vals], 
     pch='.',
     col = rgb(elevation_vals[keep_vals],
               tcw_sd_vals[keep_vals],
               nemestrina_vals[keep_vals],
               alpha = 0.2))
