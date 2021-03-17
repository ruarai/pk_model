# Parallel
library(doMC)


# Regression trees
library(gbm3)
library(dismo)

# Data manipulation
library(dplyr)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

print(paste0("Starting task ", this_task_id))

registerDoMC(cores = 4)

bootstrap_out_path <- "output/update/bootstrap_outputs/"

model_list <- readRDS(file = paste0("output/update/bootstrap_outputs/", this_task_id, "_brt_model_list.Rds"))

rename_layers_model_matrix <- function(raster){
  names(raster)[names(raster)=='layer.1'] <- 'Host_mosquito'
  names(raster)[names(raster)=='layer.2'] <- 'Host_monkey'
  names(raster)[names(raster)=='layer.3'] <- 'Host_human'
  
  return(raster)
}

seasia_covs <- brick('data/clean/raster/SEAsia_covs.grd')
seasia_covs <- dropLayer(seasia_covs, c('EVI_mean', 'EVI_SD', 'TCB_mean'))

blank_seasia <- raster('data/clean/raster/SEAsia_extent.grd')
names(blank_seasia) <- "layer"

human <- addLayer(seasia_covs, blank_seasia, blank_seasia, blank_seasia + 1)
human <- rename_layers_model_matrix(human)

prediction_covs <- human

print("Starting prediction loop...")

model_preds_seasia <- foreach(i=1:length(model_list), .packages = c('gbm3', 'dismo')) %dopar% {
  print(paste0("Predicting human-SEAsia for model ", i, " of ", length(model_list)))
  
  a <- Sys.time()
  
  m <- model_list[[i]]
  
  pred <- predict(prediction_covs, m, type="response", n.trees = length(m$trees))
  
  print(paste0("Predicted for model ", i, " in ", Sys.time() - a, " seconds."))
  
  return(pred)
}

model_preds_seasia <- getValues(stack(model_preds_seasia))

print("Saving predictions.")

saveRDS(model_preds_seasia,
        file = paste0("output/update/predictions_seasia_human/", this_task_id, "_model_pred.Rds"))







