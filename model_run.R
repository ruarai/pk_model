# Parallel
library(doMC)

# Regression trees
library(gbm3)



data_all <- read.csv("data/clean/occurrence/data_all.csv")

# let runBRT know that the host_species column is a discrete variable 

# prepare dummy prediction rasters for host, vector and monkey host species
mosquito_ras <- raster('data/clean/raster/mbs_mask.grd')
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