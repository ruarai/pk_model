# Data manipulation
library(dplyr)

library(raster)

library(Matrix)

run_unique_name <- commandArgs(trailingOnly = TRUE)[1]


pred_files <- list.files(paste0("output/update/predictions/", run_unique_name, "/"), full.names=TRUE)

n_task <- length(pred_files)

a <- Sys.time()
preds <- lapply(pred_files, function(x){
  pred_values <- readRDS(x)
  
  pred_values[is.na(pred_values)] <- 0
  
  return(Matrix::t(as(pred_values, "sparseMatrix")))
})
b <- Sys.time()
print(b - a)

n_bootstrap_per_task <- ncol(preds[[1]])

all_i <- lapply(preds,
                function(m){
                  m@i + 1
                })

all_j <- lapply(preds,
                function(m){
                  findInterval(seq(m@x)-1,m@p[-1])+1
                })

all_x <- lapply(preds,
                function(m){
                  m@x
                })

all_i_adjusted <- lapply(0:(n_task-1),
                         function(i){
                           all_i[[i + 1]] + i * n_bootstrap_per_task
                         })


full_pred_matrix <- sparseMatrix(
  i = unlist(all_i_adjusted),
  j = unlist(all_j),
  x = unlist(all_x),
  dims = c(n_bootstrap_per_task*n_task, n_bootstrap_per_task)
)


saveRDS(full_pred_matrix, 
        file = paste0("output/update/", run_unique_name , "_pred_matrix.Rds"),
        compress=FALSE)
