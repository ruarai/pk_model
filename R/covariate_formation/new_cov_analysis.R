
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

new_cov_files <- list.files("data/raw/covariate_production/new_covs",
                            full.names = TRUE)

new_cov_names <- list.files("data/raw/covariate_production/new_covs")

library(raster)


new_covs <- lapply(new_cov_files, raster)

new_covs_values <- lapply(new_covs, getValues)

new_covs_values <- do.call(cbind,new_covs_values)



non_missings <- which(!is.na(new_covs_values[,1]))


arbitrary_points <- sample(non_missings, 1000)

df <- as.data.frame(log(new_covs_values[arbitrary_points,]))

colnames(df) <- new_cov_names

plot(df, pch='.', cex=0.9, col=rgb(0,0,0,0.8))
