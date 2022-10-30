

library(raster)
library(tidyverse)
library(Matrix)

run_unique_name <- "final_1"


pred_files <- list.files(paste0("output/update/predictions/", run_unique_name, "/"), full.names=TRUE)

preds <- lapply(pred_files, function(x){
  pred_values <- readRDS(x)
  
  pred_values[is.na(pred_values)] <- 0
  
  return(Matrix::t(as(pred_values, "sparseMatrix")))
})
saveRDS(do.call(rbind, preds), 
        file = paste0("output/update/", run_unique_name , "_pred_matrix.Rds"),
        compress=FALSE)





#source("code/R/join_effect_plots.R")

rmarkdown::render('code/R_interactive/summarise_model_run.Rmd',
                  output_file = paste0('../../output/reports/report_', run_unique_name, '.html'),
                  knit_root_dir = getwd())

#rmarkdown::render('code/R_interactive/model_interpretation.Rmd',
#                  output_file = paste0('../../output/reports/report_', run_unique_name, '_iml.html'))




blank_seasia <- raster('data/clean/raster/SEAsia_extent.grd')

full_pred_matrix <- readRDS(file = paste0("output/update/", run_unique_name, "_pred_matrix.Rds"))


pred_means <- colMeans2(full_pred_matrix)
pred_means_raster <- setValues(blank_seasia, pred_means)
names(pred_means_raster) <- "mean"

pred_vars <- colVars(boot::logit(as.matrix(full_pred_matrix)))
pred_vars_raster <- setValues(blank_seasia, pred_vars)
names(pred_vars_raster) <- "var"

pred_quants <- colQuantiles(full_pred_matrix, probs = seq(from = 0, to = 1, by = 0.05))
pred_quants_raster <- brick(sapply(1:ncol(pred_quants), function(i) {
  r <- setValues(blank_seasia, pred_quants[,i])
  r[is.na(blank_seasia)] <- NA
  r
}))
names(pred_quants_raster) <- paste0("q",seq(from = 0, to = 1, by = 0.05)*100)


out_pred_brick <- brick(list(pred_means_raster, pred_vars_raster, pred_quants_raster))


writeRaster(out_pred_brick,
            "output/update/out_pred_brick_final.grd")







