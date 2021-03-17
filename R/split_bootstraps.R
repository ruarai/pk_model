library(seegSDM)
library(dplyr)

print("Running split_bootstraps.R...")

set.seed(1)

outpath <- 'output/update/'
data_all <- read.csv("data/clean/occurrence/data_all.csv")

source('code_ruarai/R/functions_parasite.R')

n_bootstraps_per_task <- 250
n_tasks <- 40

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

saveRDS(task_assignments, file = paste0(outpath, "bootstrap_inputs/", "task_assignments.Rds"))

for(unique_task_id in unique(task_assignments$task_id)){
  assignments <- task_assignments %>% filter(task_id == unique_task_id)
  
  print(paste0("Writing data for task ", unique_task_id))
  
  saveRDS(data_list[assignments$bootstrap_index],
          file = paste0(outpath, "bootstrap_inputs/", unique_task_id, "_brt_data_list.Rds"))
}

print("Done.")
