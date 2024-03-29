# Parallel
library(doMC)

# Regression trees
library(gbm3)
library(dismo)

library(dplyr)

library(seegSDM)

set.seed(1)

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_unique_name <- commandArgs(trailingOnly = TRUE)[2]
n_core <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

print(paste0("Starting task ", this_task_id))

in_dir <- paste0("output/update/bootstrap_inputs/", run_unique_name, "/")

task_assignments <- readRDS(paste0(in_dir,"task_assignments.Rds")) %>%
  filter(task_id == this_task_id)

data_list <- readRDS(paste0(in_dir, this_task_id ,"_brt_data_list.Rds"))

registerDoMC(cores = n_core)

print('Fitting Bernoulli BRT models in parallel...')
print(paste0("With ", n_core, " cores."))

model_list <- foreach(i=1:length(data_list), .packages = c('gbm3', 'dismo')) %dopar% {
  print(paste0("Fitting model ", i, " of ", length(data_list)))
  
  a <- Sys.time()
  
  bs_data <- data_list[[i]]
  
  y_index <- match("PA", names(bs_data))
  
  long_index <- match("Longitude", names(bs_data))
  lat_index <- match("Longitude", names(bs_data))
  
  host_index <- match("Host_species", names(bs_data))
  
  wt_index <- match("wt", names(bs_data))
  
  m <- runBRT(bs_data,
              gbm.x = host_index:ncol(bs_data),
              gbm.y = y_index,
              n.folds = 10,
              gbm.coords = c(long_index, lat_index),
              wt = wt_index)
  
  print(paste0("Took ", Sys.time() - a, " to fit model ", i ,"."))
  
  m
}

print('Saving model objects...')


out_dir <- paste0("output/update/bootstrap_outputs/", run_unique_name, "/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, recursive = TRUE)
}

saveRDS(model_list, 
        file = paste0(out_dir, this_task_id, "_brt_model_list.Rds"),
        compress = FALSE)

print('Saved model objects.')
