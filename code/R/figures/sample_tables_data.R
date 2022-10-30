
library(raster)
library(tidyverse)


mt_data <- read_csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv")

fs_data <- read_csv("data/raw/occurrence/polygon_data_mbs.csv") %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(Presence == 1)

fs_data_testing <- read_csv("data/raw/occurrence/presence_absence_ex-MSB_confirmed.csv") %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(Presence == 1)



get_nearest_region <- function(Longitude, Latitude, region_shp) {
  point <- tibble(Longitude, Latitude) %>%
    SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  distances <- suppressWarnings(rgeos::gDistance(point, region_shp, byid = TRUE))
  
  region_shp$ADM1_EN[[which.min(distances)]]
}

malaysia_admin1 <- shapefile('data/raw/mbs_maps/mys_admbnda_adm1_unhcr_20210211.shp')

adm2013_1 <- read_rds("data/raw/shapes_rds/admin1_shp_2013_sea.Rds")
idn_adm1 <- adm2013_1[adm2013_1$COUNTRY_ID == "IDN", ]
idn_adm1$ADM1_EN <- idn_adm1$NAME

mt_data_with_mys <- mt_data %>%
  rowwise() %>%
  mutate(mys_region = get_nearest_region(Longitude, Latitude, malaysia_admin1),
         idn_region = get_nearest_region(Longitude, Latitude, idn_adm1)) %>%
  ungroup() %>%
  mutate(Region = case_when(
    Site_country == "Malaysia" ~ mys_region,
    Site_country == "Indonesia" ~ idn_region,
    TRUE ~ Site_country
  ))




fs_data_with_mys <- fs_data %>%
  bind_rows(fs_data_testing) %>%
  rename(Site_country = Country) %>%
  rowwise() %>%
  
  mutate(mys_region = get_nearest_region(Longitude, Latitude, malaysia_admin1),
         idn_region = get_nearest_region(Longitude, Latitude, idn_adm1)) %>%
  ungroup() %>%
  mutate(Region = case_when(
    Site_country == "Malaysia" ~ mys_region,
    Site_country == "Indonesia" ~ idn_region,
    TRUE ~ Site_country
  ))

fs_data_totals <- fs_data_with_mys %>% 
  group_by(Site_country, Region) %>% 
  summarise(total_FS = n())


data_by_region <- mt_data_with_mys %>%
  
  group_by(Site_country, Region, Host, Geometry_type) %>%
  summarise(n_data = n()) %>%
  ungroup() %>%
  
  pivot_wider(names_from = c(Host, Geometry_type),
              values_from = c(n_data)) %>%
  
  full_join(fs_data_totals, by = c("Site_country", "Region")) %>%
  mutate(Region = str_to_title(Region),
         Region = str_replace(Region, "W.p.", "W.P.")) %>%
  
  arrange(Site_country, Region)


data_by_region <- mt_data_with_mys %>%
  
  group_by(Site_country, Region, Host) %>%
  summarise(n_data = n()) %>%
  ungroup() %>%
  
  pivot_wider(names_from = c(Host),
              values_from = c(n_data)) %>%
  
  full_join(fs_data_totals, by = c("Site_country", "Region")) %>%
  mutate(Region = str_to_title(Region),
         Region = str_replace(Region, "W.p.", "W.P.")) %>%
  
  arrange(Site_country, Region)


data_by_region %>%
  write_csv("data/clean/data_by_region.csv")
