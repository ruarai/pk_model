
# Map libraries
library(raster)
library(seegSDM)

# Data manipulation
library(stringr)
library(dplyr)
library(tidyr)


print("Loading raster covariate data...")

# Loading primary MBS covariate data
covs_current <- brick('data/clean/raster/mbs_raster_current.grd')
covs_temporal <- brick('data/clean/raster/mbs_raster_temporal.grd')
covs_nontemporal <- brick('data/clean/raster/mbs_raster_nontemporal.grd')

# Dropping unused layers
covs_current <- dropLayer(covs_current, c('EVI_mean', 'EVI_SD', 'TCB_mean'))
covs_nontemporal <- dropLayer(covs_nontemporal, c('EVI_mean', 'EVI_SD', 'TCB_mean'))

# Removing temporal component from our layers
names(covs_current) <- str_replace(names(covs_current), "_2012", "")

# Make a reference to the human population layer for later use
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]


writeRaster(covs_current, 
            file='data/clean/raster/covs_current',
            overwrite=TRUE)


print("Loading occurrence data...")

# Read occurrence data with polygons incorporated 
occ_mbs <- read.csv('data/clean/occurrence/polygon_data_mbs.csv', stringsAsFactors = FALSE)

# Correct column entries for mosquito host
occ_mbs[occ_mbs$Host=='mosquitoes with sporozoites', ]$Host <- 'mosquito'

# move 5 points in Singapore onto land
# find index of points outside the mask
outside_mask <- which(is.na(raster::extract(human_pop, occ_mbs[,c('Longitude', 'Latitude')])))
outside_points <- occ_mbs[outside_mask,]
outside_points <- outside_points[, c('Longitude', 'Latitude')]
land_points <- nearestLand(outside_points, human_pop, 10000)

# replace all outside_mask points with lats/longs for land points
for (i in 1:length(outside_mask)) {
  occ_mbs[outside_mask[[i]], c('Longitude', 'Latitude')] <- land_points[i,]
}

# change ID column name to Unique_ID
occ_mbs <- occ_mbs %>% rename(Unique_ID = ID)

# get extent of covs_current
ext <- extent(covs_current)

# find index of all points falling outside the extent
outside_ext_idx <- which((occ_mbs$Latitude < ext[3]) 
                         |(occ_mbs$Latitude > ext[4]) 
                         |(occ_mbs$Longitude < ext[1]) 
                         |(occ_mbs$Longitude > ext[2]))

stopifnot(length(outside_ext_idx)==0)

# subset the presence and absence points 
absence <- occ_mbs %>% filter(Presence == 0)
occ <- occ_mbs %>% filter(Presence == 1)

write.csv(occ_mbs, "data/clean/occurrence/MBS_occ_abs.csv")


print("Loading PK_Merged...")



pk_merged = read.csv('data/raw/occurrence/Pk_Merged_MT.csv')

# Drop rows with missing Long/Lat
pk_merged <- pk_merged %>%
  drop_na(Longitude, Latitude)

pk_outside_MBS <- is.na(raster::extract(human_pop, 
                                        pk_merged[,c('Longitude', 'Latitude')]))

pk_merged <- pk_merged[!pk_outside_MBS,]

occ_update <- pk_merged %>% filter(Presence == 1)

# Remove duplicate studies
occ_update <- occ_update %>% distinct(Latitude, Longitude, Start_year, .keep_all=T)

# Long! Removing rows without an exclusion reason
occ_update <- occ_update %>%
  filter(is.na(Exclusion.reason..1.No.data.2.Data.from.other.studies.or.in.original.3.Data.downloaded.from.health.dept.etc.4.too.aggregated.not.specific.enough.5.diagnostics.6.absence))

# Creating a Year field
# The floor of the mean of start and end year
# Set to 2012 if null (-999)
occ_update <- occ_update %>%
  rowwise() %>%
  mutate(Year = floor(mean(Start_year,End_year))) %>%
  mutate(Year = case_when(Year == -999 ~ 2012,
                          TRUE ~ Year))

# Rename macaque to monkey
occ_update <- occ_update %>%
  mutate(Host = case_when(Host == "macaque" ~ "monkey",
                          TRUE ~ Host))

write.csv(occ_update, "data/clean/occurrence/Pk_merged_occurrence.csv")
  

print("Sorting background data...")


# load occurrence data without coords for polygons 

poly_occ <- read.csv('data/clean/occurrence/parasite_data_mbs_bigpolys_excluded.csv', stringsAsFactors = FALSE)

# Mosquitoes w/ sporozoites -> just mosquitoes
poly_occ <- poly_occ %>%
  mutate(Host = case_when(Host == "mosquitoes with sporozoites" ~ "mosquito",
                          TRUE ~ Host))

poly_occ <- poly_occ %>%
  filter(Presence == 1)
  
# Determine the number of each host
host_counts <- data.frame(host_name = c("human", "mosquito","monkey"), counts = numeric(3), points = numeric(3))
host_counts <- host_counts %>%
  rowwise() %>%
  mutate(counts = poly_occ %>%
           filter(Host == host_name) %>%
           nrow())

background_point_n <- 6000
total_presence_n <- sum(host_counts$counts)

host_counts <- host_counts %>%
  rowwise() %>%
  mutate(points = counts / total_presence_n * background_point_n)


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


occ_all <- bind_rows(occ, occ_update) %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host) %>%
  mutate(PA = 1,
         wt = 1)

bg_all <- background_data %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host) %>%
  mutate(PA = 0,
         wt = 0.5)



data_samples <- bind_rows(occ_all, bg_all)


data_samples <- data_samples %>%
  mutate(Year_Constrained = pmin(pmax(Year, year_min), year_max)) #

write.csv(data_samples, "data/clean/occurrence/data_samples.csv")



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



data_samples <- data_samples %>% 
  mutate(Host_species = recode(Host,
                               mosquito = 1,
                               monkey = 2,
                               human = 3))


host_matrix <- model.matrix(~0 + as.factor(data_samples$Host_species))

colnames(host_matrix) <- c("Host_mosquito", "Host_monkey", "Host_human")

data_all <- cbind(data_samples, data_covs, host_matrix)

write.csv(data_all, "data/clean/occurrence/data_all.csv")
