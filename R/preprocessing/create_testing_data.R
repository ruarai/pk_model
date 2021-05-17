
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")

test_data_FS <- read.csv("data/raw/occurrence/presence_absence_ex-MSB_confirmed.csv")

test_data_MT <- read.csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv")



test_data_all <- bind_rows(test_data_FS)#, test_data_MT)



test_points <- SpatialPoints(test_data_all %>% select("Longitude", "Latitude"),
                             proj4string = crs(sea_mask))

over_mbs <- over(test_points, as(MBS_simple, 'Spatial'))

test_data <- test_data_all %>%
  filter(is.na(over_mbs$COUNTRY_ID)) %>%
  rename(Unique_ID = ID) %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host, Admin_level,
         Gaul_code, Polygon_code) %>%
  filter(Admin_level != 'C')

test_points <- test_data %>%
  filter(Geometry_type == 'point')

test_poly <- test_data %>%
  filter(Geometry_type == 'polygon')


shape_files <- list(
  gadm_adm1 = readRDS("data/raw/shapes_rds/admin1_shp_gadm.Rds"),
  gadm_adm2 = readRDS("data/raw/shapes_rds/admin2_shp_gadm.Rds"),
  knowlesi_range = readRDS("data/raw/shapes_rds/knowlesi_range_shp.Rds")
)


shape_from_point <- function(shape, point){
  matches <- over(shape, point)
  
  return(shape[!is.na(matches),])
}

get_shapes <- function(uncoded_poly_df){
  shape_list <- list()
  
  for(i in 1:nrow(uncoded_poly_df)) {
    poly_data <- uncoded_poly_df[i,]
    poly_point <- SpatialPoints(poly_data %>% select(Longitude, Latitude),
                                proj4string = crs("+proj=longlat +datum=WGS84 +no_defs "))
    
    poly_shape <- NULL
    if(poly_data$Admin_level == "1" | poly_data$Admin_level == "Admin1"){
      poly_shape <- st_simplify(st_as_sf(shape_from_point(shape_files[['gadm_adm1']], poly_point)))
    } else if(poly_data$Admin_level == "2" | poly_data$Admin_level == "Admin2"){
      poly_shape <- st_simplify(st_as_sf(shape_from_point(shape_files[['gadm_adm2']], poly_point)))
    } else if(poly_data$Admin_level == "knowlesi_range"){
      kr <- shape_files[['knowlesi_range']]
      poly_shape <- st_simplify(st_as_sf(kr[kr$ID == poly_shape$Polygon_code,]))
    }
    
    if(!is.null(poly_shape))
      shape_list <- c(shape_list, list(poly_shape))
  }
  
  shape_list <- lapply(shape_list, function(x) {as(x,'Spatial')})
  shape_list <- lapply(shape_list, function(x) {as(x,'SpatialPolygons')})
  
  return(do.call(rbind, c(shape_list, list(makeUniqueIDs= TRUE))))
}


poly_shapes <- get_shapes(test_poly %>% filter(Admin_level == "Admin1"))
