


# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

test_points <- read.csv("data/clean/occurrence/testing/test_points.csv")
stopifnot(all(test_points$PA == 1))


test_poly_data <- read.csv("data/clean/occurrence/testing/test_poly.csv")

test_poly_shape <- readRDS("data/clean/occurrence/testing/poly_shapes.Rds")
rownames(test_poly_data) <- names(test_poly_shape)

test_poly_sdf <- SpatialPolygonsDataFrame(test_poly_shape, test_poly_data)
test_poly_sdf$PA <- factor(test_poly_sdf$PA)

blank_seasia <- raster('data/clean/raster/SEAsia_extent.grd')

blank_seasia[] <- NA

test_points_spatial <- SpatialPoints(test_points[,c("Longitude", "Latitude")])

ix_seasia <- setValues(blank_seasia,1:ncell(blank_seasia)) 

points_ix <- raster::extract(ix_seasia,
                             test_points_spatial)

poly_ix <- raster::extract(ix_seasia,
                           test_poly_sdf)

testing_data <-
  
  map_dfr(
    1:nrow(test_poly_sdf),
    function(i) {
      if(length(poly_ix[[i]]) == 0) {
        return(tibble())
      }
      
      tibble(
        ix = poly_ix[[i]],
        PA = test_poly_data$PA[i],
        Year = test_poly_data$Year[i],
        Host = test_poly_data$Host[i]
      )
    }
  ) %>%
  bind_rows(
    tibble(
      ix = points_ix,
      PA = 1,
      
      Year = test_points$Year,
      Host = test_points$Host
    )
  ) %>%
  
  group_by(ix) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  
  arrange(ix)

year_min <- 2001
year_max <- 2019


testing_data <- testing_data %>%
  mutate(Year_Constrained = pmin(pmax(Year, year_min), year_max))



covs_current <- brick('data/clean/raster_updated/prediction_SEA.grd')

covs_temporal <- brick('data/clean/raster_updated/temporal_SEA.grd')
covs_nontemporal <- brick('data/clean/raster_updated/nontemporal_SEA.grd')

data_covs <- raster::extract(covs_current, testing_data$ix)
data_covs[] <- NA


for (year in year_min:year_max) {
  
  # Get temporal covariates for our current year
  covs_year <- covs_temporal[[str_which(names(covs_temporal), str_c("_", year))]]
  
  # Remove year tags from our covariates
  names(covs_year) <- str_replace(names(covs_year), str_c("_", year), "")
  
  # Add non-temporal covariates
  covs_year <- addLayer(covs_year, covs_nontemporal)
  
  ix_year <- testing_data %>%
    filter(Year_Constrained == year) %>%
    pull(ix)
  
  # extract data
  covs_year_extract <- raster::extract(covs_year, ix_year)
  
  # check they're all there
  stopifnot(all(colnames(data_covs) %in% colnames(covs_year_extract)))
  
  # match up the column names so they're in the right order
  match <- match(colnames(data_covs), colnames(covs_year_extract))
  
  data_covs[testing_data$Year_Constrained == year,] <- covs_year_extract[, match]
}

outside_idx <- attr(na.omit(data_covs), 'na.action')



if(!is.null(outside_idx)){
  data_covs <- data_covs[-outside_idx,]
  testing_data <- testing_data[-outside_idx,]
}



testing_data <- testing_data %>% 
  mutate(Host_species = recode(Host,
                               mosquito = 1,
                               monkey = 2,
                               human = 3))


testing_data_full <- bind_cols(
  xyFromCell(blank_seasia, testing_data$ix) %>% `colnames<-`(c("Longitude", "Latitude")) %>% as_tibble(),
  testing_data,
  as_tibble(data_covs)
)

testing_data_full %>%
  write_rds("data/clean/testing_data_full.rds")


