
#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


# Map libraries
library(raster)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)

covs_current <- brick('data/clean/raster/mbs_raster_current.grd')
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]


print("Loading occurrence data...")

# Read occurrence data with polygons incorporated 
mbs_occ_abs <- read.csv('data/raw/occurrence/polygon_data_mbs.csv')

# Correct column entries for mosquito host
mbs_occ_abs[mbs_occ_abs$Host=='mosquitoes with sporozoites', ]$Host <- 'mosquito'

# move 5 points in Singapore onto land
# find index of points outside the mask
outside_mask <- which(is.na(raster::extract(human_pop, mbs_occ_abs[,c('Longitude', 'Latitude')])))
outside_points <- mbs_occ_abs[outside_mask,]
outside_points <- outside_points[, c('Longitude', 'Latitude')]
land_points <- nearestLand(outside_points, human_pop, 10000)

# replace all outside_mask points with lats/longs for land points
for (i in 1:length(outside_mask)) {
  mbs_occ_abs[outside_mask[[i]], c('Longitude', 'Latitude')] <- land_points[i,]
}

# change ID column name to Unique_ID
mbs_occ_abs <- mbs_occ_abs %>% rename(Unique_ID = ID)

# get extent of covs_current
ext <- extent(covs_current)

# find index of all points falling outside the extent
outside_ext_idx <- which((mbs_occ_abs$Latitude < ext[3]) 
                         |(mbs_occ_abs$Latitude > ext[4]) 
                         |(mbs_occ_abs$Longitude < ext[1]) 
                         |(mbs_occ_abs$Longitude > ext[2]))

stopifnot(length(outside_ext_idx)==0)

# subset the presence and absence points 
occ <- mbs_occ_abs %>%
  filter(Presence == 1) %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host)

write.csv(occ, "data/clean/occurrence/pk_present/MBS_FS_B_2005-2014.csv", row.names = FALSE)



subset_occ <- read.csv('data/raw/occurrence/parasite_data_mbs_bigpolys_excluded.csv', stringsAsFactors = FALSE)

# Mosquitoes w/ sporozoites -> just mosquitoes
subset_occ <- subset_occ %>%
  mutate(Host = case_when(Host == "mosquitoes with sporozoites" ~ "mosquito",
                          TRUE ~ Host))

subset_occ <- subset_occ %>%
  filter(Presence == 1) %>%
  rename(Unique_ID = ID) %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host) 

write.csv(subset_occ, "data/clean/occurrence/MBS_FS_A_2005-2014.csv", row.names = FALSE)



