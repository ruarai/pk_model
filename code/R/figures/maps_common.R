
library(raster)
library(tidyverse)

source("code/R/figures/bbox_to_sp.R")

sea_mask <- raster("data/clean/raster_updated/extent_SEA")
sea_mask[sea_mask == 0] <- 1

sea_area <- bbox_to_SpatialPolygons(extent(sea_mask),
                                    proj4string = crs(sea_mask))


SEA_country_codes <- read.csv("data/raw/admin_maps/SEA_country_codes.txt")[,1]

SEA_simple <- readRDS("data/raw/admin_maps/SEA_simple.Rds")

MBS_simple <- SEA_simple[SEA_simple$COUNTRY_ID %in% c("MYS", "BRN", "SGP"),]

mbs_mask <- raster("data/clean/raster_updated/extent_MBS")




colors_risk <- c("#55843b", "#a4be79","#ffffbf", "#921d67", "#6c0043")


