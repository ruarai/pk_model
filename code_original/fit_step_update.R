# run knowlesi parasite malaysia/brunei/singapore model
# includes human, monkey and vector infection data
# pseudo-absences for monkey infection data were sampled from monkey and mammal presence points
# pseudo-absences for vector infection data were sampled from human pop layer
# uses a joint distribution to assess difference in distribution of parasite in humans, monkeys and vectors 

# clear workspace
#rm(list = ls())

library(seegSDM)
library(raster)
#library(snowfall)
library(dismo)
library(rgdal)
library(foreach)
library(doMC)
library(gbm3)

# setwd("~/Desktop/knowlesi")
# setwd("/data/cephfs/punim1228/knowlesi")


source('code/functions_parasite.R')

# set the RNG seed
set.seed(1)

# ~~~~~~~~~~~~~~~~~

# set output path
outpath <- 'output/update/'

# ~~~~~~~~~~~~~~~~~

message("load raster data")

# raster covariates
covs_current <- brick('data/clean/raster/mbs_raster_current.grd')
covs_temporal <- brick('data/clean/raster/mbs_raster_temporal.grd')
covs_nontemporal <- brick('data/clean/raster/mbs_raster_nontemporal.grd')

# drop correlated covariates and reservoir and vector species
covs_current <- dropLayer(covs_current, c('EVI_mean', 'EVI_SD', 'TCB_mean'))
covs_nontemporal <- dropLayer(covs_nontemporal, c('EVI_mean', 'EVI_SD', 'TCB_mean'))

# rename the temporal layers (remove year)
names(covs_current)[names(covs_current)=='forest_intact_2012']<-'forest_intact'
names(covs_current)[names(covs_current)=='forest_disturbed_2012']<-'forest_disturbed'
names(covs_current)[names(covs_current)=='open_shrublands_2012']<-'open_shrublands'
names(covs_current)[names(covs_current)=='woody_savannas_2012']<-'woody_savannas'
names(covs_current)[names(covs_current)=='savannas_2012']<-'savannas'
names(covs_current)[names(covs_current)=='grasslands_2012']<-'grasslands'
names(covs_current)[names(covs_current)=='croplands_2012']<- 'croplands'
names(covs_current)[names(covs_current)=='cropland_natural_vegetation_mosaic_2012']<-'cropland_natural_vegetation_mosaic'
names(covs_current)[names(covs_current)=='permanent_wetlands_2012']<- 'permanent_wetlands'
names(covs_current)[names(covs_current)=='fascicularis_2012']<- 'fascicularis'
names(covs_current)[names(covs_current)=='nemestrina_2012']<- 'nemestrina'
names(covs_current)[names(covs_current)=='leucosphyrus_group_2012']<- 'leucosphyrus_group'

# get the population raster
human_pop <- covs_current[[which(names(covs_current)=='human_pop')]]

# ~~~~~~~~~~~~~~~~~
message("load occurrence data")

# occurrence data with polygons incorporated 
occ_mbs <- read.csv('data/clean/occurrence/polygon_data_mbs.csv', stringsAsFactors = FALSE)

# correct column entries for mosquito host
occ_mbs[occ_mbs$Host=='mosquitoes with sporozoites',]$Host <- 'mosquito'

# move 5 points in Singapore onto land
# find index of points outside the mask
outside_mask <- which(is.na(extract(human_pop, occ_mbs[,c('Longitude', 'Latitude')])))
outside_points <- occ_mbs[outside_mask,]
outside_points <- outside_points[, c('Longitude', 'Latitude')]
land_points <- nearestLand(outside_points, human_pop, 10000)

# replace all outside_mask points with lats/longs for land points
for (i in 1:length(outside_mask)) {
  occ_mbs[outside_mask[[i]], c('Longitude', 'Latitude')] <- land_points[i,]
}

# change ID column name to Unique_ID
colnames(occ_mbs)[colnames(occ_mbs)=='ID'] <- 'Unique_ID'

# get extent of covs_current
ext <- extent(covs_current)

# find index of all points falling outside the extent
outside_ext_idx <- which((occ_mbs$Latitude < ext[3]) 
                         |(occ_mbs$Latitude > ext[4]) 
                         |(occ_mbs$Longitude < ext[1]) 
                         |(occ_mbs$Longitude > ext[2]))

stopifnot(length(outside_ext_idx)==0)

# subset the presence and absence points 
absence <- subset(occ_mbs, occ_mbs$Presence==0)
occ <- subset(occ_mbs, occ_mbs$Presence==1)

# ~~~~~~~~~~~~~~~~
message("load pk_merged")

pk_merged = read.csv('data/raw/occurrence/Pk_Merged.csv')
# remove empty rows:
isna.idx = which(is.na(pk_merged$Longitude))
pk_merged = pk_merged[-isna.idx,]

# find index of all points falling outside MBS mask
outside_idx <- which(is.na(extract(human_pop, 
                                   pk_merged[,c('Longitude', 'Latitude')])))
occ_mbs_update = pk_merged[-outside_idx,]
# outside_mbs_occ_update = pk_merged[outside_idx,]

# discard absences ?
occ_update <- subset(occ_mbs_update, occ_mbs_update$Presence==1)
occ_update = occ_update[which(!duplicated(occ_update[,c("Latitude", "Longitude","Start_year")])),]
isna.idx = which(!is.na(occ_update$Exclusion.reason..1.No.data.2.Data.from.other.studies.or.in.original.3.Data.downloaded.from.health.dept.etc.4.too.aggregated.not.specific.enough.5.diagnostics.6.absence))
occ_update = occ_update[-isna.idx,]

# Year column: use mean of start year / end year, rounded down
# if no entry, use 2012
year = floor(rowMeans(data.frame(occ_update$Start_year, 
                                 occ_update$End_year)))
year[which(year == -999)] = 2012
occ_update$Year = year

occ_update$Unique_ID = NA

host = occ_update$Host
levels(host)[levels(host)=="macaque"] <- "monkey"
occ_update$Host = host

# ~~~~~~~~~~~~~~~~
message("sort background data")
# load occurrence data without coords for polygons 

occ_raw <- read.csv('data/clean/occurrence/parasite_data_mbs_bigpolys_excluded.csv', stringsAsFactors = FALSE)
occ_raw[occ_raw$Host=='mosquitoes with sporozoites',]$Host <- 'mosquito'

# subset the presence points 
presence <- subset(occ_raw, occ_raw$Presence==1)
absences2 <- subset(occ_raw, occ_raw$Presence==0)

# find out number of infections in humans, mosquitos and monkeys in occurrence dataset
human_pres <- nrow(subset(presence, presence$Host=='human'))
#human_abs <- nrow(subset(absences2, absences2$Host=='human'))
vector_pres <- nrow(subset(presence, presence$Host=='mosquito'))
#vector_abs <- nrow(subset(absences2, absences2$Host=='mosquito'))
monkey_pres <- nrow(subset(presence, presence$Host=='monkey'))
#monkey_abs <- nrow(subset(absences2, absences2$Host=='monkey'))
total_pres <- human_pres + vector_pres + monkey_pres
#total_abs <- human_abs + vector_abs + monkey_abs

# set total number of background points
bg <- 6000

# calculate number of human, vector and monkey points based on total number of background points
human_points <- round(human_pres/total_pres*bg)
vector_points <-round(vector_pres/total_pres*bg)
monkey_points <- round(monkey_pres/total_pres*bg)

# load background datasets
fascicularis <- read.csv(paste(getwd(), '/data/clean/occurrence/fascicularis_presence.csv', sep = ""), stringsAsFactors=FALSE)
nemestrina <- read.csv(paste(getwd(), '/data/clean/occurrence/nemestrina_presence.csv', sep = ""), stringsAsFactors = FALSE)
monkey_bias <- read.csv(paste(getwd(), '/data/clean/occurrence/mammals-bias.csv', sep = ""), stringsAsFactors = FALSE)

# remove extra columns in fascicularis dataset 
fascicularis <- subset(fascicularis, select= -c(9:19))

# change column names of mammal-bias dataset
colnames(monkey_bias)[colnames(monkey_bias)=='decimalLatitude'] <- 'Latitude'
colnames(monkey_bias)[colnames(monkey_bias)=='decimalLongitude'] <- 'Longitude'
colnames(monkey_bias)[colnames(monkey_bias)=='year'] <- 'Year'

# add geometry-type column
monkey_bias$Geometry_type <- 'point'

# use presence data only
fascicularis <- subset(fascicularis, fascicularis$Presence==1)
nemestrina <- subset(nemestrina, nemestrina$Presence==1)

# make column names match
colnames(fascicularis)[colnames(fascicularis)=='Geometry_t'] <- 'Geometry_type'
colnames(nemestrina)[colnames(nemestrina)=='Geometry_t'] <- 'Geometry_type'

# combine monkey datasets
monkeys <- rbind(fascicularis[, c('Longitude', 'Latitude', 'Year', 'Geometry_type')], 
                 nemestrina[, c('Longitude', 'Latitude', 'Year', 'Geometry_type')],
                 monkey_bias[, c('Longitude', 'Latitude', 'Year', 'Geometry_type')]) 

# remove any points that don't fall on covariate mask
template <- covs_current[[1]]
monkeys <- insideMask(monkeys, template)

# sample background points from monkey and vector datasets and combine
monkey_bg <- monkeys[sample(1:nrow(monkeys), monkey_points, replace=FALSE),]
monkey_bg$Host <- 'monkey'

# generate pseudo-absence points for human and vector infection points
human_bg <- bgSample (human_pop, n=human_points, prob=TRUE, replace= TRUE, spatial=FALSE)
vector_bg <- bgSample (human_pop, n=vector_points, prob=TRUE, replace=TRUE, spatial=FALSE)

# convert to a dataframe and add relevant columns 
human_background <- data.frame(human_bg, stringsAsFactors = FALSE)
colnames(human_background)[colnames(human_background)=='x'] <- 'Longitude'
colnames(human_background)[colnames(human_background)=='y'] <- 'Latitude'
human_background$Year <- 2012
human_background$Geometry_type <- 'point'
human_background$Host <- 'human'

vector_background <- data.frame(vector_bg, stringsAsFactors = FALSE)
colnames(vector_background)[colnames(vector_background)=='x'] <- 'Longitude'
colnames(vector_background)[colnames(vector_background)=='y'] <- 'Latitude'
vector_background$Year <- 2012
vector_background$Geometry_type <- 'point'
vector_background$Host <- 'mosquito'

background <- rbind(human_background, vector_background, monkey_bg)

# add Unique_ID column for later to deal with polygon data in occurrence data set
background$Unique_ID <- NA

# ~~~~~~~~~~~~~~~~~

# combine the occurrence and background records, exclude "true" absence records
dat <- rbind(cbind(PA = rep(1, nrow(occ) + nrow(occ_update)),
                   wt = rep(1, nrow(occ) + nrow(occ_update)),
                   rbind(occ[, c('Unique_ID', 'Longitude', 'Latitude', 'Year', 'Geometry_type', 'Host')],
                     occ_update[, c('Unique_ID', 'Longitude', 'Latitude', 'Year', 'Geometry_type', 'Host')])),
             cbind(PA = rep(0, nrow(background)),
                   wt = 0.5,
                   background[ ,c('Unique_ID', 'Longitude', 'Latitude', 'Year','Geometry_type', 'Host')]))

# ~~~~~~~~~~~~~~~~~

message("extract covariate values for each data point")

# create an empty dataframe
dat_covs <- extract(covs_current, dat[, c('Longitude', 'Latitude')])

dat_covs[] <- NA

# loop through extracting covs for all other years
for (year in 2001:2012) {
  
  # truncate years to the ones we have covariates for
  years <- dat$Year
  years <- pmax(years, 2001)
  years <- pmin(years, 2012)
  
  # index for this year's data
  idx <- which(years == year)
  
  # covs for this year
  
  # get index for temporal covariates for relevant year
  
  idx2 <- which (substr(names(covs_temporal),
                        nchar(names(covs_temporal)) - 3,
                        nchar(names(covs_temporal)))==year)
  
  
  covs_year <- covs_temporal[[idx2]]
  names(covs_year)[names(covs_year)==paste0('forest_intact_', year)]<-'forest_intact'
  names(covs_year)[names(covs_year)==paste0('forest_disturbed_', year)]<-'forest_disturbed'
  names(covs_year)[names(covs_year)==paste0('open_shrublands_', year)]<-'open_shrublands'
  names(covs_year)[names(covs_year)==paste0('woody_savannas_', year)]<-'woody_savannas'
  names(covs_year)[names(covs_year)==paste0('savannas_', year)]<-'savannas'
  names(covs_year)[names(covs_year)==paste0('grasslands_', year)]<-'grasslands'
  names(covs_year)[names(covs_year)==paste0('permanent_wetlands_', year)]<-'permanent_wetlands'
  names(covs_year)[names(covs_year)==paste0('croplands_', year)]<-'croplands'
  names(covs_year)[names(covs_year)==paste0('cropland_natural_vegetation_mosaic_', year)]<-'cropland_natural_vegetation_mosaic'
  names(covs_year)[names(covs_year)==paste0('fascicularis_', year)]<-'fascicularis'
  names(covs_year)[names(covs_year)==paste0('nemestrina_', year)]<-'nemestrina'
  names(covs_year)[names(covs_year)==paste0('leucosphyrus_group_', year)]<-'leucosphyrus_group'
  
  # add nontemporal covariates
  covs_year <- addLayer(covs_year, covs_nontemporal)
  
  # extract data
  covs_year_extract <- extract(covs_year, dat[idx, c('Longitude', 'Latitude')])
  
  # check they're all there
  stopifnot(all(colnames(dat_covs) %in% colnames(covs_year_extract)))
  
  # match up the column names so they're in the right order
  match <- match(colnames(dat_covs), colnames(covs_year_extract))
  
  # extract covariates for all points
  dat_covs[idx, ] <- covs_year_extract[, match]
  
}                                                                               


# add host_species column and make it numeric
dat$Host_species <- NA

dat$Host_species[dat$Host=='mosquito']<- 1
dat$Host_species[dat$Host=='monkey']<- 2
dat$Host_species[dat$Host=='human']<- 3

# combine covariates with the other info and host species (one hot encoded)
dat_all = cbind(dat, dat_covs, model.matrix(~0 + as.factor(dat$Host_species)))
names(dat_all) = c(names(dat), colnames(dat_covs), 
                   "Host_mosquito", "Host_monkey", "Host_human")

# let runBRT know that the host_species column is a discrete variable 
#dat_all$Host_species <- factor(dat_all$Host_species)
# (ended up going for one hot encoding here)

# prepare dummy prediction rasters for host, vector and monkey host species
mosquito_ras <- raster('data/clean/raster/mbs_mask.grd')
#mosquito_ras <- raster('data/clean/raster/mbs_raster_nontemporal.grd')
monkey_ras <- mosquito_ras #+ 1
human_ras <- mosquito_ras #+ 2
blank_ras <- mosquito_ras - 1

# add each layer to covs_current to make a covariate set for each species for model prediction
mosquito <- addLayer(covs_current, mosquito_ras, blank_ras, blank_ras)
names(mosquito)[names(mosquito)=='layer.1'] <- 'Host_mosquito'
names(mosquito)[names(mosquito)=='layer.2'] <- 'Host_monkey'
names(mosquito)[names(mosquito)=='layer.3'] <- 'Host_human'
monkey <- addLayer(covs_current, blank_ras, monkey_ras, blank_ras)
names(monkey)[names(monkey)=='layer.1'] <- 'Host_mosquito'
names(monkey)[names(monkey)=='layer.2'] <- 'Host_monkey'
names(monkey)[names(monkey)=='layer.3'] <- 'Host_human'
human <- addLayer(covs_current, blank_ras, blank_ras, human_ras)
names(human)[names(human)=='layer.1'] <- 'Host_mosquito'
names(human)[names(human)=='layer.2'] <- 'Host_monkey'
names(human)[names(human)=='layer.3'] <- 'Host_human'

# check which rows in dat_covs contain an NA i.e. missing covariates (outside mask)
outside_idx <- attr(na.omit(dat_covs), 'na.action')

stopifnot(is.null(outside_idx))

# ~~~~~~~~~~~~~~~~~
# run bootstrapped BRT models

ncpu <- 10
nboot <- ncpu*1

# 195 total presence records
# ftable(dat_all[dat_all$PA == 1,]$Geometry_type)
# 62 point records, 133 polygon records
# get random bootstraps of the data (minimum 10 pres/10 abs)
data_list <- replicate(nboot,
                       subsamplePolys(dat_all,
                                      minimum = c(10, 10),
                                      replace = TRUE),
                       simplify = FALSE)

data_list <- lapply(data_list,
                    balanceWeights2)

save(data_list, file=paste0(outpath, "brt_data_list.RData"))
# m = dismo::gbm.step(data_list[[i]],
#                     gbm.x = 9:ncol(data_list[[1]]),
#                     gbm.y = 1,
#                     n.folds = 10,
#                     max.trees = 20000,
#                     #shrinkage = 0.001,
#                     learning.rate = 0.001,
#                     tree.complexity = 4,
#                     step.size = 10,
#                     keep.fold.models = TRUE, 
#                     keep.fold.vector = TRUE,
#                     keep.fold.fit = TRUE,
#                     family = 'bernoulli') # are tree complexity and interaction depth equivalent?
#                     # wt

# https://stackoverflow.com/questions/18640169/subscript-out-of-bounds-in-gbm-function
# soln: use model.matrix() to one hot encode HostSpecies variable
# fit bernoulli BRT models in parallel
registerDoMC()
getDoParWorkers()

message('fit bernoulli BRT models in parallel')

model_list <- foreach(i=1:length(data_list), .packages = c('gbm3', 'dismo')) %dopar% {
  m <- gbm3::gbm(formula = PA ~ TCB_SD + human_pop + TCW_mean + TCW_SD +
                   SRTM_elevation + urban_access + Pf_temp +
                   forest_intact + forest_disturbed +
                   open_shrublands + woody_savannas +
                   savannas + grasslands + permanent_wetlands +
                   croplands + cropland_natural_vegetation_mosaic +
                   fascicularis + nemestrina + leucosphyrus_group +
                   Host_human + Host_mosquito + Host_monkey,
                 data = data_list[[i]],
                 distribution = "bernoulli",
                 cv.fold = 10,
                 shrinkage = 0.005,
                 n.trees = 20000,
                 weights = data_list[[i]][,2],
                 interaction.depth = 4) # equiv. to tree complexity
  message(paste0("fitting model ", i, " of ", length(data_list)))
  m
}

message('saving model objects')
save(model_list, file = paste0(outpath, "brt_model_list.RData"))


