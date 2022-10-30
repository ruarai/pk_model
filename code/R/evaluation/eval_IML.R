
require(tidyverse)
require(gbm)
require(iml)


source("code/R/evaluation/ROC_fns.R")

ix <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_name <- commandArgs(trailingOnly = TRUE)[2]

print(paste0("Starting task ", ix))

model_list <- read_rds(
  paste0("output/update/bootstrap_outputs/", run_name, "/", ix, "_brt_model_list.Rds")
)


data_list <- read_rds(
  paste0("output/update/bootstrap_inputs/", run_name, "/", ix, "_brt_data_list.Rds")
)

print("Loaded data...")

results_full <- purrr::map(
  1:length(model_list),
  function(i) {
    print(paste0("Running for list index ", i))
    
    model <- model_list[[i]]
    X <- data_list[[i]][,10:ncol(data_list[[i]])]
    
    model_iml <- Predictor$new(model$model, data = X, y = data_list[[i]]$PA)
    importance <- FeatureImp$new(model_iml, loss = "mae")
    
    ale_full <- map_dfr(
      model$relinf$var,
      function(i_var) {
        ale <- FeatureEffect$new(model_iml, feature = i_var)
        ale$results %>%
          `colnames<-`(c(".type", ".value", "x")) %>%
          mutate(var = i_var)
      }
    )
    
    list("importance" = importance$results,
         "ale" = ale_full)
    
})

results_full  %>%
  write_rds(paste0("output/update/model_stats/", run_name, "/", ix, "_testing_IML.rds"))

print("Finished")

