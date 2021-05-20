

library(raster)
library(tidyverse)
library(Matrix)

run_unique_name <- "buildup_4"


pred_files <- list.files(paste0("output/update/predictions/", run_unique_name, "/"), full.names=TRUE)

preds <- lapply(pred_files, function(x){
  pred_values <- readRDS(x)
  
  pred_values[is.na(pred_values)] <- 0
  
  return(Matrix::t(as(pred_values, "sparseMatrix")))
})
saveRDS(do.call(rbind, preds), 
        file = paste0("output/update/", run_unique_name , "_pred_matrix.Rds"),
        compress=FALSE)


source("code/R/join_effect_plots.R")

rmarkdown::render('code/R_interactive/summarise_model_run.Rmd',
                  output_file = paste0('../../output/reports/report_', run_unique_name, '.html'))

rmarkdown::render('code/R_interactive/model_interpretation.Rmd',
                  output_file = paste0('../../output/reports/report_', run_unique_name, '.html'))
