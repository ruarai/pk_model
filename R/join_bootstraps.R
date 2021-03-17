# Parallel
library(doMC)

# Regression trees
library(gbm3)
library(dismo)

# Data manipulation
library(dplyr)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

print(paste0("Starting task ", this_task_id))


bootstrap_out_path <- "output/update/bootstrap_outputs/"

model_list <- readRDS(file = paste0("output/update/bootstrap_outputs/", this_task_id, "_brt_model_list.Rds"))

task_assignments <- readRDS(paste0("output/update/bootstrap_inputs/task_assignments.Rds"))

n_boot <- nrow(task_assignments)

mbs_land <- raster('data/clean/raster/mbs_mask.grd')
blank_raster <- mbs_land - 1

rename_layers_model_matrix <- function(raster){
  names(raster)[names(raster)=='layer.1'] <- 'Host_mosquito'
  names(raster)[names(raster)=='layer.2'] <- 'Host_monkey'
  names(raster)[names(raster)=='layer.3'] <- 'Host_human'
  
  return(raster)
}


covs_current <- brick("data/clean/raster/covs_current.grd")

# add each layer to covs_current to make a covariate set for each species for model prediction
mosquito <- addLayer(covs_current, mbs_land, blank_raster, blank_raster)
mosquito <- rename_layers_model_matrix(mosquito)

monkey <- addLayer(covs_current, blank_raster, mbs_land, blank_raster)
monkey <- rename_layers_model_matrix(monkey)

human <- addLayer(covs_current, blank_raster, blank_raster, mbs_land)
human <- rename_layers_model_matrix(human)

prediction_covs <- human

registerDoMC(cores = detectCores())

print('Producing predictions for BRT models in parallel...')

model_preds <- foreach(i=1:length(model_list), .packages = c('gbm3', 'dismo')) %dopar% {
  print(paste0("Predicting for model ", i, " of ", length(model_list)))
  
  m <- model_list[[i]]
  
  predict(prediction_covs, m, type="response", n.trees = length(m$trees))
}

model_preds <- stack(model_preds)
n_layer <- length(names(model_preds))
n_cell <- ncell(model_preds)

model_partial_summary <- foreach(i=1:ncell(model_preds)) %dopar% {
  if(i %% 10000 == 0){
    print(paste0("Summarising at pixel ", i, " of ", n_cell))
  }
  
  # Get the ith pixel across all layers
  pixel_vals <- model_preds[[1:n_layer]][i]
  
  list(partial_mean = sum(pixel_vals)/n_boot)
}





