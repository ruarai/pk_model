
#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


# Map libraries
library(raster)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)


sabah <- read.csv('data/raw/occurrence/Geocode_10.9.20_Pk_indigenous.csv') %>%
  select(year, pcr_provisional, x_long_final, y_lat_final, )

# Only including strong Pk PCR results
sabah <- sabah %>%
  filter(pcr_provisional == "Pk") %>%
  select(-pcr_provisional)


#Unique_ID, Longitude, Latitude, Year, Geometry_type, Host
sabah$Unique_ID <- NA
sabah$Geometry_type <- "point"
sabah$Host <- "human"

sabah <- sabah %>%
  rename(Longitude = x_long_final,
         Latitude = y_lat_final) 

write.csv(sabah, "data/clean/occurrence/pk_present/Sabah_RT_2015-2020.csv", row.names = FALSE)
