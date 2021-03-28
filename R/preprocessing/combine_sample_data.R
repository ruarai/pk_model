
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

source('code_ruarai/R/functions_parasite.R')

# Inclusive max/min
year_min <- 2001
year_max <- 2012

# Map libraries
library(raster)
library(seegSDM)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)


occ_files <- c("MBS_FS_B_2005-2014.csv")#,
               #"MBS_MT_point_2007-2018.csv",
               #"MBS_MT_polygon_2007-2018.csv")

occ_files <- str_c("data/clean/occurrence/pk_present/", occ_files)


# Need to ensure unique IDs remain unique when binding the datasets together

clean_unique_ids <- function(unique_ids){
  distinct_unique_ids <- unique(unique_ids)
  replacement_unique_ids <- 1:length(distinct_unique_ids)
  
  sapply(unique_ids, function(i){
    replacement_unique_ids[match(i, distinct_unique_ids)]
  })
}

occ_data <- lapply(occ_files, read.csv)

occ_data <- lapply(occ_data, function(x) {
  x$Unique_ID <- clean_unique_ids(x$Unique_ID)
  x
})

max_uid <- max(sapply(occ_data,function(x) max(x$Unique_ID)))

# Separate out each dataset by the maximum possible spacing
for(i in 1:length(occ_data)) {
  occ_data[[i]]$Unique_ID <- occ_data[[i]]$Unique_ID + (i - 1) * (max_uid + 1)
}

occ_data <- bind_rows(occ_data)




covs_current <- brick('data/clean/raster/covs_current')
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]


covs_temporal <- brick('data/clean/raster/mbs_raster_temporal.grd')
covs_nontemporal <- brick('data/clean/raster/mbs_raster_nontemporal.grd')

# Dropping unused layers
covs_current <- dropLayer(covs_current, c('EVI_mean', 'EVI_SD', 'TCB_mean'))
covs_nontemporal <- dropLayer(covs_nontemporal, c('EVI_mean', 'EVI_SD', 'TCB_mean'))


# Psuedoabsence data generation:

bg_occ <- read.csv("data/clean/occurrence/MBS_FS_A_2005-2014.csv")

# Determine the number of each host
host_counts <- data.frame(host_name = c("human", "mosquito","monkey"), counts = numeric(3), points = numeric(3))
host_counts <- host_counts %>%
  rowwise() %>%
  mutate(counts = bg_occ %>%
           filter(Host == host_name) %>%
           nrow())

background_point_n <- 6000
total_presence_n <- sum(host_counts$counts)

host_counts <- host_counts %>%
  rowwise() %>%
  mutate(points = round(counts / total_presence_n * background_point_n))


fascicularis <- read.csv('data/clean/occurrence/fascicularis_presence.csv')
nemestrina <- read.csv('data/clean/occurrence/nemestrina_presence.csv')
monkey_bias <- read.csv('data/clean/occurrence/mammals-bias.csv')

# Remove NA columns
fascicularis <- fascicularis %>%
  select(-c(9:18))

monkey_bias <- monkey_bias %>%
  rename(Latitude = decimalLatitude,
         Longitude = decimalLongitude,
         Year = year)

monkey_bias$Geometry_type <- 'point'

fascicularis <- fascicularis %>%
  filter(Presence == 1)
nemestrina <- nemestrina %>%
  filter(Presence == 1)

fascicularis <- fascicularis %>%
  rename(Geometry_type = Geometry_t)


monkey_data <- bind_rows(fascicularis, nemestrina, monkey_bias) %>%
  select(Longitude, Latitude, Year, Geometry_type)

cov_mask <- covs_current[[1]]
monkey_data <- insideMask(monkey_data, cov_mask)

monkey_bg <- monkey_data[sample(1:nrow(monkey_data),
                                host_counts %>% filter(host_name == "monkey") %>% pull(points), # yuk
                                replace=FALSE),]
monkey_bg$Host <- 'monkey'



human_bg <- bgSample (human_pop,
                      n=host_counts %>% filter(host_name == "human") %>% pull(points),
                      prob=TRUE, replace= TRUE, spatial=FALSE)

vector_bg <- bgSample (human_pop,
                       n=host_counts %>% filter(host_name == "mosquito") %>% pull(points),
                       prob=TRUE, replace=TRUE, spatial=FALSE)

# Function to manipulate out points data into usable form
points_to_df <- function(points, host){
  data.frame(points) %>%
    mutate(Year = 2012,
           Geometry_type = 'point',
           Host = host) %>%
    rename(Longitude = x,
           Latitude = y)
}


background_data <- bind_rows(points_to_df(human_bg, "human"),
                             points_to_df(vector_bg, "mosquito"),
                             monkey_bg)
background_data$Unique_ID <- NA


occ_all <- occ_data %>%
  mutate(PA = 1,
         wt = 1)

bg_all <- background_data %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host) %>%
  mutate(PA = 0,
         wt = 1)



data_samples <- bind_rows(occ_all, bg_all)


data_samples <- data_samples %>%
  mutate(Year_Constrained = pmin(pmax(Year, year_min), year_max))



print("Extracting covariate values for each data point...")

data_covs <- raster::extract(covs_current, data_samples[, c('Longitude', 'Latitude')])
data_covs[] <- NA


for (year in year_min:year_max) {
  
  # Get temporal covariates for our current year
  covs_year <- covs_temporal[[str_which(names(covs_temporal), str_c("_", year))]]
  
  # Remove year tags from our covariates
  names(covs_year) <- str_replace(names(covs_year), str_c("_", year), "")
  
  # Add non-temporal covariates
  covs_year <- addLayer(covs_year, covs_nontemporal)
  
  long_lats_year <- data_samples %>%
    filter(Year_Constrained == year) %>%
    select(Longitude, Latitude)
  
  # extract data
  covs_year_extract <- raster::extract(covs_year, long_lats_year)
  
  # check they're all there
  stopifnot(all(colnames(data_covs) %in% colnames(covs_year_extract)))
  
  # match up the column names so they're in the right order
  match <- match(colnames(data_covs), colnames(covs_year_extract))
  
  data_covs[data_samples$Year_Constrained == year,] <- covs_year_extract[, match]
}

# check which rows in data_covs contain an NA i.e. missing covariates (outside mask)
outside_idx <- attr(na.omit(data_covs), 'na.action')
stopifnot(is.null(outside_idx))


# Don't perform model matrix creation.

data_samples <- data_samples %>% 
  mutate(Host_species = recode(Host,
                               mosquito = 1,
                               monkey = 2,
                               human = 3))


data_all <- cbind(data_samples, data_covs)

write.csv(data_all, "data/clean/occurrence/data_all.csv", row.names = FALSE)
