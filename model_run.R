# Parallel
library(doMC)

# Regression trees
library(gbm3)
library(dismo)

# Data manipulation
library(stringr)

outpath <- 'output/update/'


data_all <- read.csv("data/clean/occurrence/data_all.csv")



# ~~~~~~~~~~~~~~~~~
# run bootstrapped BRT models


# 195 total presence records
# 62 point records, 133 polygon records
# get random bootstraps of the data (minimum 10 pres/10 abs)
data_list <- replicate(nboot,
                       subsamplePolys(data_all,
                                      minimum = c(10, 10),
                                      replace = TRUE),
                       simplify = FALSE)

data_list <- lapply(data_list,
                    balanceWeights2)

save(data_list, file=str_c(outpath, "brt_data_list.RData"))
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

print('Fitting Bernoulli BRT models in parallel...')

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
                 n.trees = 200,
                 weights = data_list[[i]]$w,
                 interaction.depth = 4) # equiv. to tree complexity
  print(paste0("Fitting model ", i, " of ", length(data_list)))
  m
}

print('Saving model objects')

save(model_list, file = paste0(outpath, "brt_model_list.RData"))
