

require(tidyverse)
require(gbm)
require(dismo)
require(gbm3)


source("code/R/evaluation/ROC_fns.R")

ix <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
run_name <- commandArgs(trailingOnly = TRUE)[2]

print(paste0("Starting task ", ix))

testing_data_full <- read_rds("data/clean/testing_data_full.rds")
model_list <- read_rds(
  paste0("output/update/bootstrap_outputs/", run_name, "/", ix, "_brt_model_list.Rds")
)


print('Loaded model list...')

model_stats <- map_dfr(
  model_list,
  function(i_model) {
    getStats2(
      i_model,
      testing_data_full
    ) %>% t() %>% as_tibble()
  }
) 

model_stats  %>%
  write_csv(paste0("output/update/model_stats/", run_name, "/", ix, "_testing_stats.csv"))



print('Evaluation complete.')

