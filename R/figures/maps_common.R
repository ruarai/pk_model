
library(raster)
library(tidyverse)
library(sf)

source("code_ruarai/R/figures/bbox_to_sp.R")

adm0_shape <- shapefile("data/raw/admin_maps/admin2013_0")

sea_mask <- raster("data/clean/raster_updated/extent_SEA")
sea_mask[sea_mask == 0] <- 1

sea_area <- bbox_to_SpatialPolygons(extent(sea_mask),
                                    proj4string = crs(sea_mask))

SEA_countries <- suppressWarnings(!is.na(over(adm0_shape, sea_area)))


SEA_shape <- adm0_shape[SEA_countries,]
SEA_country_codes <- SEA_shape$COUNTRY_ID

SEA_simple <- suppressWarnings(st_simplify(st_as_sf(SEA_shape), dTolerance = 0.01))

MBS_simple <- SEA_simple[SEA_simple$COUNTRY_ID %in% c("MYS", "BRN", "SGP"),]

mbs_mask <- raster("data/clean/raster_updated/extent_MBS")




colors_risk <- c("#55843b", "#a4be79","#ffffbf", "#921d67", "#6c0043")


