# Parallel
library(doMC)

# Interpretable machine learning
library(iml)

# BRTs
library(gbm)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_unique_name <- commandArgs(trailingOnly = TRUE)[2]
n_core <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

model_list <- readRDS("output/update/bootstrap_outputs/with_new_covs_2/1_brt_model_list.Rds")


data_for_model <- readRDS("output/update/bootstrap_inputs/with_new_covs_2/1_brt_data_list.Rds")

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
    
    X <- data_for_model[,10:ncol(data_for_model)]
    
    pred_agnostic = Predictor$new(m_gbm$model, data = X, y = data_for_model$PA)
    
    
    print(paste0("Producing shapley values for model ", i))
    
    
    
    pred_shapley <- Shapley$new(pred_agnostic, X, sample.size = 5)
    
    
    print(paste0("Producing shapley values in time ", Sys.time() - a))
    
    return(pred_shapley)
  }



print("Saving shapley values.")

out_dir_shapley <- paste0("output/update/shapley_values/", run_unique_name, "/")
if(!dir.exists(out_dir_shapley)){
  dir.create(out_dir_shapley, recursive = TRUE)
}


saveRDS(shapley_results,
        file = paste0(out_dir_shapley, this_task_id, "_shapley_values.Rds"),
        compress=FALSE)












