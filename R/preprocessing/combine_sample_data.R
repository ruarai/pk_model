
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

# Map libraries
library(raster)
library(seegSDM)

source('code_ruarai/R/functions_parasite.R')

# Inclusive max/min
year_min <- 2001
year_max <- 2019

set.seed(2021-04-26)


# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)

source("code_ruarai/R/preprocessing/polygon_thinning.R")

source("code_ruarai/R/preprocessing/create_background_data.R")

occ_all <- read.csv("data/clean/occurrence/pk_present/ALL_occ_thinned.csv")


covs_current <- brick('data/clean/raster_updated/prediction_MBS.grd')

covs_temporal <- brick('data/clean/raster_updated/temporal_MBS.grd')
covs_nontemporal <- brick('data/clean/raster_updated/nontemporal_MBS.grd')


bg_all <- read.csv("data/clean/occurrence/ALL_background_points.csv")

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
#stopifnot(is.null(outside_idx))

data_covs <- data_covs[-outside_idx,]
data_samples <- data_samples[-outside_idx,]

# Don't perform model matrix creation.

data_samples <- data_samples %>% 
  mutate(Host_species = recode(Host,
                               mosquito = 1,
                               monkey = 2,
                               human = 3))


data_all <- cbind(data_samples, data_covs)

write.csv(data_all, "data/clean/occurrence/data_all.csv", row.names = FALSE)
