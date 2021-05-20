# Map libraries
library(raster)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)



pk_merged <- read.csv("data/raw/occurrence/Pk_merged_uncoded.csv")

admin_region_records <- pk_merged %>%
  filter(Geometry_type == 'polygon',
         Admin_level %in% c('1','2','3'))



# Downsample level 3 to 2
admin_region_records[admin_region_records$Admin_level == '3',]$Admin_level <- 2

point_records <- pk_merged %>%
  filter(Geometry_type == 'point')




source("code/R/figures/maps_common.R")
source("code/R/figures/admin_common.R")



# Time to replicate records for each point within a polygon:


human_pop <- brick('data/clean/raster/mbs_raster_current.grd')[[3]]

new_records <- list()

for(i in 1:nrow(admin_region_records)) {
  record <- admin_region_records[i,]
  
  # Give it an ID
  record$ID <- i
  
  if(record$Admin_level == 1) {
    shape <- get_polygons_1(record %>% select("Longitude", "Latitude"))
    
    
  }else if(record$Admin_level == 2) {
    shape <- get_polygons_2(record %>% select("Longitude", "Latitude"))
  }
  
  shape_points <- rasterToPoints(rasterize(shape, human_pop))[,c("x","y")]
  
  record_repeated <- record %>% slice(rep(1,nrow(shape_points)))
  
  record_repeated$Longitude <- shape_points[,c("x")]
  record_repeated$Latitude <- shape_points[,c("y")]
  
  
  new_records <- c(new_records, list(record_repeated))
}

admin_region_records <- bind_rows(new_records)



admin_region_records <- admin_region_records %>%
  rename(Unique_ID = ID) %>%
  select(Year,
         Longitude, Latitude,
         Unique_ID,
         Geometry_type,
         Host)


covs_current <- brick('data/clean/raster/mbs_raster_current.grd')
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]
outside_mask <- is.na(raster::extract(human_pop, admin_region_records[,c('Longitude', 'Latitude')]))
admin_region_records <- admin_region_records[!outside_mask,]

write.csv(admin_region_records,
          "data/clean/occurrence/pk_present/MBS_MT_polygon_2007-2018.csv",
          row.names = FALSE)

point_records <- point_records %>%
  rename(Unique_ID = ID) %>%
  select(Year,
         Longitude, Latitude,
         Unique_ID,
         Geometry_type,
         Host)



write.csv(point_records, "data/clean/occurrence/pk_present/MBS_MT_point_2007-2018.csv", row.names = FALSE)



