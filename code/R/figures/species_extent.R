# Map libraries
library(raster)
library(tidyverse)


pred_brick <- brick("data/clean/raster_updated/prediction_SEA")




resvecext <- 1 -
  ((1 - pred_brick[['fascicularis']]) *
   (1 - pred_brick[['nemestrina']]))


plot(resvecext * pred_brick[['leucosphyrus_group']] > 0)
