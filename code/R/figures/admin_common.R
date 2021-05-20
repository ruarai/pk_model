
library(raster)
library(tidyverse)
library(sf)



admin1_shp <- shapefile('data/raw/gadm_maps/gadm36_1.shp')
admin2_shp <- shapefile('data/raw/gadm_maps/gadm36_2.shp')
admin1_sea <- admin1_shp[admin1_shp$GID_0 %in% SEA_country_codes,]
admin2_sea <- admin2_shp[admin2_shp$GID_0 %in% SEA_country_codes,]



get_polygons_1 <- function(longlat) {
  poly_points <- SpatialPoints(longlat,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  gids <- over(poly_points, admin1_sea)$GID_1
  gids <- gids[!is.na(gids)]
  matches <- sapply(gids, function(x) admin1_sea[admin1_sea$GID_1 == x,])
  shapes <- suppressWarnings(st_simplify(st_as_sf(do.call(rbind,matches)), dTolerance = 0.01))
}

get_polygons_2 <- function(longlat) {
  poly_points <- SpatialPoints(longlat,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  gids <- over(poly_points, admin2_sea)$GID_2
  gids <- gids[!is.na(gids)]
  matches <- sapply(gids, function(x) admin2_sea[admin2_sea$GID_2 == x,])
  
  shapes <- suppressWarnings(st_simplify(st_as_sf(do.call(rbind,matches)), dTolerance = 0.01))
}
