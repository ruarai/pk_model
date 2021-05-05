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




table(admin_region_records$Site_name)

malaysia_admin1 <- shapefile('data/raw/mbs_maps/mys_admbnda_adm1_unhcr_20210211')
malaysia_admin2 <- shapefile('data/raw/mbs_maps/mys_admbnda_adm2_unhcr_20210211')




admin_region_records$adm1_name_match <- match(admin_region_records$Site_name,
                                           malaysia_admin1$ADM1_EN)

admin_region_records$adm2_name_match <- match(admin_region_records$Site_name,
                                           malaysia_admin2$ADM2_EN)


records_points <- SpatialPoints(admin_region_records[,c("Longitude","Latitude")],
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

adm1_point_matches <- over(records_points, malaysia_admin1)
admin_region_records$adm1_point_match <- match(adm1_point_matches$ADM1_EN,
                                               malaysia_admin1$ADM1_EN)

adm2_point_matches <- over(records_points, malaysia_admin2)
admin_region_records$adm2_point_match <- match(adm2_point_matches$ADM2_EN,
                                               malaysia_admin2$ADM2_EN)

adm1_names <- malaysia_admin1$ADM1_EN
adm2_names <- malaysia_admin2$ADM2_EN

# Proving that our point matches are more valid than our name matches:
for(i in 1:nrow(admin_region_records)) {
  record <- admin_region_records[i,]
  
  if(record$Admin_level == 1) {
    if(!is.na(record$adm1_name_match)) {
      same_match <- record$adm1_name_match == record$adm1_point_match
      
      if(!same_match){
        print(str_c("Match adm1 failed for '", record$Site_name,
                    "', with name match '", adm1_names[record$adm1_name_match],
                    "' and point match '", adm1_names[record$adm1_point_match],"'"))
      }
      
    }
  }
  
  if(record$Admin_level == 2) {
    if(!is.na(record$adm2_name_match)) {
      same_match <- record$adm2_name_match == record$adm2_point_match
      
      if(!same_match){
        print(str_c("Match adm2 failed for '", record$Site_name,
                    "', with name match '", adm2_names[record$adm2_name_match],
                    "' and point match '", adm2_names[record$adm2_point_match],"'"))
        
        print(str_c("Site notes: ", record$Notes_site))
        
        
        plot(malaysia_admin1)
        plot(malaysia_admin2[record$adm2_name_match,], add=TRUE, col='blue')
        points(records_points[record$adm2_point_match,], col='red')
        
        #readline()
      }
    }
  }
}
# Remove now unnecessary name matches
admin_region_records <- admin_region_records %>%
  select(-c(adm1_name_match, adm2_name_match))


admin_region_records_naive <- admin_region_records %>%
  rename(Unique_ID = ID) %>%
  select(Year,
         Longitude, Latitude,
         Unique_ID,
         Geometry_type,
         Host)

write.csv(admin_region_records_naive, "data/clean/occurrence/pk_present/MBS_MT_polygon_unexpanded_2007-2018.csv", row.names = FALSE)



# Time to replicate records for each point within a polygon:


human_pop <- brick('data/clean/raster/mbs_raster_current.grd')[[3]]

admin_region_records$Longitude <- NA
admin_region_records$Latitude <- NA

new_records <- list()

for(i in 1:nrow(admin_region_records)) {
  record <- admin_region_records[i,]
  
  # Give it an ID
  record$ID <- i
  
  if(record$Admin_level == 1) {
    shape <- malaysia_admin1[record$adm1_point_match,]
    
    
  }else if(record$Admin_level == 2) {
    shape <- malaysia_admin2[record$adm2_point_match,]
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
          "data/raw/occurrence/Pk_merged_coded.csv",
          row.names = FALSE)

point_records <- point_records %>%
  rename(Unique_ID = ID) %>%
  select(Year,
         Longitude, Latitude,
         Unique_ID,
         Geometry_type,
         Host)



write.csv(point_records, "data/clean/occurrence/pk_present/MBS_MT_point_2007-2018.csv", row.names = FALSE)



