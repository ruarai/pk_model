library(raster)
library(tidyverse)




pred_raster <- brick("data/clean/raster_updated/prediction_SEA")


macaque <- 1 - (1 - pred_raster[['fascicularis']]) *
  (1 - pred_raster[['nemestrina']])

species_extent <- macaque * pred_raster[['leucosphyrus_group']] > 0

writeRaster(species_extent,
            "data/clean/raster_updated/reservoir_vector_extent.grd",
            overwrite=TRUE)
