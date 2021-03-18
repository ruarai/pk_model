library(seegSDM)
library(dplyr)

print("Running split_bootstraps.R...")

set.seed(1)

data_all <- read.csv("data/clean/occurrence/data_all.csv")

source('code_ruarai/R/functions_parasite.R')


run_unique_name <- commandArgs(trailingOnly = TRUE)[1]


n_bootstraps_per_task <- 10
n_tasks <- 10

nboot <- n_bootstraps_per_task * n_tasks

print("Subsampling polygons...")

# 195 total presence records
# 62 point records, 133 polygon records
# get random bootstraps of the data (minimum 10 pres/10 abs)
data_list <- replicate(nboot,
                       subsamplePolys(data_all,
                                      minimum = c(10, 10),
                                      replace = TRUE),
                       simplify = FALSE)

print("Balancing weights...")

data_list <- lapply(data_list,
                    balanceWeights2)

print("Saving...")

task_assignments <- data.frame(cpu_id = rep(1:n_bootstraps_per_task, nrep=n_tasks),
                               task_id = rep(1:n_tasks, each=n_bootstraps_per_task),
                               bootstrap_index = 1:(n_tasks*n_bootstraps_per_task))



out_dir <- paste0("output/update/bootstrap_inputs/", run_unique_name, "/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, recursive = TRUE)
}

saveRDS(task_assignments, file = paste0(out_dir, "task_assignments.Rds"),
        compress = FALSE)

for(unique_task_id in unique(task_assignments$task_id)){
  assignments <- task_assignments %>% filter(task_id == unique_task_id)
  
  print(paste0("Writing data for task ", unique_task_id))
  
  saveRDS(data_list[assignments$bootstrap_index],
          file = paste0(out_dir, unique_task_id, "_brt_data_list.Rds"),
          compress = FALSE)
}

print("Done.")
