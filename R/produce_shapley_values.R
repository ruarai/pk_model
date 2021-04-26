# Parallel
library(doMC)

# Interpretable machine learning
library(iml)

# BRTs
library(gbm)

# Rasters
library(raster)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_unique_name <- commandArgs(trailingOnly = TRUE)[2]
n_core <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

model_list <- readRDS("output/update/bootstrap_outputs/with_new_covs_2/1_brt_model_list.Rds")


data_for_model <- readRDS("output/update/bootstrap_inputs/with_new_covs_2/1_brt_data_list.Rds")


mbs_pred <- brick("data/clean/raster_updated/prediction_MBS")
mbs_values <- getValues(mbs_pred)

mbs_cells <- which(!is.na(mbs_values[,1]))

mbs_data <- mbs_values[mbs_cells,]
mbs_pred <- data.frame(mbs_data)
mbs_pred$Host_species <- 3

data_eg <- data_for_model[[1]]

mbs_pred <- mbs_pred[,match(names(data_eg[, 10:ncol(data_eg)]), names(mbs_pred))]


registerDoMC(cores = n_core)
n_model <- length(model_list)

shapley_results <- foreach(i=1:n_model, 
                           .packages = c('iml',
                                         'gbm')) %dopar%
  {
    print(paste0("Producing agnostic predictor for model ", i))
    
    a <- Sys.time()
    
    model_i <- model_list[[i]]
    
    data_i <- data_for_model[[i]]
    
    X <- data_i[,10:ncol(data_i)]
    
    pred_agnostic = Predictor$new(model_i$model, data = X, y = data_for_model$PA)
    
    
    print(paste0("Producing shapley values for model ", i))
    
    
    
    pred_shapley <- Shapley$new(pred_agnostic, X, sample.size = 1)
    
    
    print(paste0("Producing shapley explainations for model ", i))
    
    
    
    cell_shapley <- lapply(1:length(mbs_cells), function(i){
      if(i %% 100 == 0){
        print(i)
      }
      
      pred_shapley$explain(mbs_pred[i,])
      
      pred_shapley$results
    })
    
    
    
    print(paste0("Produced shapley explainations in time ", Sys.time() - a))
    
    return(cell_shapley)
  }



print("Saving shapley values.")

out_dir_shapley <- paste0("output/update/shapley_values/", run_unique_name, "/")
if(!dir.exists(out_dir_shapley)){
  dir.create(out_dir_shapley, recursive = TRUE)
}


saveRDS(shapley_results,
        file = paste0(out_dir_shapley, this_task_id, "_shapley_values.Rds"),
        compress=FALSE)












