
setwd("/data/gpfs/projects/punim1449/knowlesi_ruarai")

library(iml)

library(tidyverse)

library(Matrix)

shap_vals <- readRDS("output/update/shapley_values/with_new_covs_2/1_shapley_values.Rds")

n_bs <- length(shap_vals)
n_cell <- length(shap_vals[[1]])
n_var <- nrow(shap_vals[[1]][[1]])

feature_shaps <- lapply(1:n_var, function(i_var){
  Matrix(nrow = n_cell, ncol = n_bs)
})


for(i_bs in 1:10){
  
  shap_val_set <- shap_vals[[i_bs]]
  
  for(i_cell in 1:n_cell){
    shap_cell <- shap_val_set[[i_cell]]
    
    for(i_var in 1:1){
      feature_shaps[[i_var]][i_cell, i_bs] <- shap_cell$phi[i_var]
    }
  }
}


all_shap_data <- all_shap_data %>%
  group_by(feature, i_cell) %>%
  summarise(phi = mean(phi))




mbs_raster <- raster("data/clean/raster_updated/prediction_MBS")

mbs_values <- getValues(mbs_raster)

mbs_cells <- which(!is.na(mbs_values))

vals_mbs <- vector("numeric", length = ncell(mbs_raster))
vals_mbs[mbs_cells] <- feature_shaps[[1]][,1]

mbs_raster <- setValues(mbs_raster, vals_mbs)







