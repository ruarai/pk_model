
#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


# Map libraries
library(raster)
library(seegSDM)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)


print("Loading raster covariate data...")

# Loading primary MBS covariate data
covs_current <- brick('data/clean/raster/mbs_raster_current.grd')

# Removing temporal component from our layers
names(covs_current) <- str_replace(names(covs_current), "_2012", "")

# Make a reference to the human population layer for later use
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]


writeRaster(covs_current, 
            file='data/clean/raster/covs_current',
            overwrite=TRUE)

