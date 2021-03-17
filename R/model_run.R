# Parallel
library(doMC)

# Regression trees
library(gbm3)
library(dismo)

library(dplyr)

library(seegSDM)

set.seed(1)

outpath <- 'output/update/'

this_task_id <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

print(paste0("Starting task ", this_task_id))

task_assignments <- readRDS(paste0("output/update/bootstrap_inputs/task_assignments.Rds")) %>%
  filter(task_id == this_task_id)

data_list <- readRDS(paste0(outpath, "/bootstrap_inputs/", this_task_id ,"_brt_data_list.Rds"))

n_core <- 4
registerDoMC(cores = n_core)

print('Fitting Bernoulli BRT models in parallel...')
print(paste0("With ", n_core, " cores."))

model_list <- foreach(i=1:length(data_list), .packages = c('gbm3', 'dismo')) %dopar% {
  print(paste0("Fitting model ", i, " of ", length(data_list)))
  
  a <- Sys.time()
  
  m <- runBRT(data_list[[i]],
              gbm.x = 9:ncol(data_list[[1]]),
              gbm.y = 1,
              n.folds = 10,
              gbm.coords = 4:5,
              wt = 2)
  
  print(paste0("Took ", Sys.time() - a, " to fit model ", i ,"."))
  
  m
}

print('Saving model objects...')

saveRDS(model_list, 
        file = paste0(outpath,"bootstrap_outputs/", this_task_id, "_brt_model_list.Rds"),
        compress = FALSE)

print('Saved model objects.')
