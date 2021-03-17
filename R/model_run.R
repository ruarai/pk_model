# Parallel
library(doMC)

# Regression trees
library(gbm3)
library(dismo)

library(dplyr)

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
  
  m <- gbm3::gbm(formula = PA ~ TCB_SD + human_pop + TCW_mean + TCW_SD +
                   SRTM_elevation + urban_access + Pf_temp +
                   forest_intact + forest_disturbed +
                   open_shrublands + woody_savannas +
                   savannas + grasslands + permanent_wetlands +
                   croplands + cropland_natural_vegetation_mosaic +
                   fascicularis + nemestrina + leucosphyrus_group +
                   Host_human + Host_mosquito + Host_monkey,
                 data = data_list[[i]],
                 distribution = "bernoulli",
                 cv.fold = 10,
                 shrinkage = 0.005,
                 n.trees = 200,
                 weights = data_list[[i]]$w,
                 interaction.depth = 4) # equiv. to tree complexity
  m
}

print('Saving model objects...')

saveRDS(model_list, 
        file = paste0(outpath,"bootstrap_outputs/", this_task_id, "_brt_model_list.Rds"),
        compress = FALSE)

print('Saved model objects.')
