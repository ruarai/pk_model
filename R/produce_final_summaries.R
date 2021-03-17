# Data manipulation
library(dplyr)

library(raster)

library(Matrix)

#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update/")



mbs_land <- raster('data/clean/raster/mbs_mask.grd')
blank_raster <- mbs_land - 1


pred_files <- list.files("output/update/predictions_mbs_human/", full.names=TRUE)

preds <- lapply(pred_files, function(x){
  pred_values <- readRDS(x)
  
  pred_values[is.na(pred_values)] <- 0
  
  return(as(t(pred_values), "sparseMatrix"))
})

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

all_i_adjusted <- lapply(0:621,
                         function(i){
                           all_i[[i + 1]] + i * 16
                         })


full_pred_matrix <- sparseMatrix(
  i = unlist(all_i_adjusted),
  j = unlist(all_j),
  x = unlist(all_x),
  dims = c(16*622, 76128)
)


saveRDS(full_pred_matrix, file = "output/update/full_mbs_human.Rds")