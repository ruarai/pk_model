# Parallel
library(doMC)


# Regression trees
library(gbm3)
library(dismo)

# Data manipulation
library(dplyr)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_unique_name <- commandArgs(trailingOnly = TRUE)[2]
n_core <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

print(paste0("Starting task ", this_task_id))

registerDoMC(cores = n_core)

in_dir <- paste0("output/update/bootstrap_outputs/", run_unique_name, "/")
model_list <- readRDS(file = paste0(in_dir, this_task_id, "_brt_model_list.Rds"))

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
print(paste0("With ", n_core, " cores."))


model_preds_seasia <- foreach(i=1:length(model_list), 
                              .packages = c('dismo',
                                            'snowfall',
                                            'seegSDM')) %dopar%
  {
    print(paste0("Predicting human-SEAsia for model ", i, " of ", length(model_list)))
    
    a <- Sys.time()
    
    m <- model_list[[i]]
    
    pred <- predict(prediction_covs, m$model, type="response", n.trees = m$model$n.trees)
    
    print(paste0("Predicted for model ", i, " in ", Sys.time() - a, "."))
    
    return(pred)
    
  }


model_preds_seasia <- getValues(stack(model_preds_seasia))

# get cv statistics
stat_lis <- lapply(model_list, getStats)

# convert the stats list into a matrix using the do.call function
stats <- do.call("rbind", stat_lis)

# save the relative influence scores
relinf_list <- lapply(model_list, function(x) {
  as.data.frame(t(x$model$contributions[,2, drop=FALSE]))
})

relinf <- bind_rows(relinf_list)




print("Saving predictions.")




out_dir_stats <- paste0("output/update/model_stats/", run_unique_name, "/")
if(!dir.exists(out_dir_stats)){
  dir.create(out_dir_stats, recursive = TRUE)
}
write.csv(stats,paste0(out_dir_stats,'stats.csv'))

out_dir_rel_inf <- paste0("output/update/model_rel_inf/", run_unique_name, "/")
if(!dir.exists(out_dir_rel_inf)){
  dir.create(out_dir_rel_inf, recursive = TRUE)
}
write.csv(relinf, file = paste0(out_dir_rel_inf, 'relative_influence.csv'))


out_dir_predictions <- paste0("output/update/predictions/", run_unique_name, "/")
if(!dir.exists(out_dir_predictions)){
  dir.create(out_dir_predictions, recursive = TRUE)
}

saveRDS(model_preds_seasia,
        file = paste0(out_dir_predictions, this_task_id, "_model_pred.Rds"),
        compress=FALSE)