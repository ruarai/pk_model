
#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


# Map libraries
library(raster)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)


pk_merged = read.csv('data/raw/occurrence/Pk_Merged_MT.csv')

pk_merged_col_names_clean <- c("ID",
                               "Start_month",
                               "Start_year",
                               "End_month",
                               "End_year",
                               "Publication_year",
                               "Sampling_motivation",
                               "Host",
                               "Community",
                               "No_gen_tested",
                               "No_Pk_positive",
                               "Method_gen_tested_calculated",
                               "Diagnostic_1",
                               "Diagnostic_2",
                               "Diagnostic_3",
                               "Presence",
                               "Exclusion_code",
                               "Notes_study",
                               "Site_country",
                               "Site_name",
                               "Notes_site",
                               "Geometry_type",
                               "Latitude",
                               "Longitude",
                               "Admin_level",
                               "Gaul_code",
                               "Polygon_code",
                               "Centroid_latitude",
                               "Centroid_longitude",
                               "DOI",
                               "Source_primary",
                               "Source_secondary",
                               "Map_point_code",
                               "Inclusion")

colnames(pk_merged) <- pk_merged_col_names_clean

pk_merged <- pk_merged[,1:(ncol(pk_merged)-5)]



papers <- pk_merged %>%
  pull(Source_primary) %>%
  unique()

print(str_c(length(papers), " full text papers sourced."))


pk_include <- pk_merged %>%
  filter(Inclusion == 1 &
           is.na(Exclusion_code)) %>%
  filter(Presence != 0)


papers_included <- pk_include %>%
  pull(Source_primary) %>%
  unique()

print(str_c(length(papers_included), " full text papers included."))
  


records_without_longlat <- pk_include %>% 
  filter(is.na(Longitude) | is.na(Latitude))

print(str_c("Excluding ", nrow(records_without_longlat), " records for missing longitude/latitude."))

pk_include <- pk_include %>%
  drop_na(Longitude, Latitude)


# Remove duplicate studies
pk_include <- pk_include %>% distinct(Latitude, Longitude, Start_year, .keep_all=T)


# Creating a Year field
# The floor of the mean of start and end year
# Set to 2012 if null (-999)
pk_include <- pk_include %>%
  rowwise() %>%
  mutate(Year = floor(mean(Start_year,End_year))) %>%
  mutate(Year = case_when(Year == -999 ~ 2012,
                          TRUE ~ Year))

# Rename macaque to monkey
pk_include <- pk_include %>%
  mutate(Host = case_when(Host == "macaque" ~ "monkey",
                          TRUE ~ Host))

write.csv(pk_include ,"data/raw/occurrence/Pk_merged_uncoded_SEA.csv", row.names=FALSE)




human_pop <- brick('data/clean/raster/mbs_raster_current.grd')[[3]]

pk_outside_MBS <- is.na(raster::extract(human_pop, 
                                        pk_include[,c('Longitude', 'Latitude')]))
points_outside_MBS <- pk_include[,c('Longitude', 'Latitude')][pk_outside_MBS,]

plot(human_pop,
     xlim=c(min(points_outside_MBS$Longitude)-10, max(points_outside_MBS$Longitude)+10),
     ylim=c(min(points_outside_MBS$Latitude)-10, max(points_outside_MBS$Latitude)+10))
points(points_outside_MBS)
title("Points excluded for not being on MBS land.")


pk_include <- pk_include[!pk_outside_MBS,]

plot(human_pop)
points(pk_include[,c('Longitude', 'Latitude')],
       cex=0.5)
title("Points and polygons included for analysis.")



write.csv(pk_include ,"data/raw/occurrence/Pk_merged_uncoded.csv", row.names=FALSE)




# Exclusion analysis:



exclusion_analysis <- pk_merged %>%
  mutate(Exclusion_code = str_trim(Exclusion_code)) %>%
  mutate(Exclusion_code = str_replace(Exclusion_code, "5", "4")) %>%
  filter(Exclusion_code != '') %>%
  group_by(Source_primary) %>%
  summarise(exclusions = str_c(unique(Exclusion_code), collapse=' '))



# Maps:





















