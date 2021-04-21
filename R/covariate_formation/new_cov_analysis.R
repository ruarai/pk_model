
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

new_cov_files <- list.files("data/raw/covariate_production/gee_covs/", pattern="*.tif",
                            full.names = TRUE)

new_cov_names <- list.files("data/raw/covariate_production/gee_covs/", pattern="*.tif")

library(raster)

blank <- raster("data/clean/raster/SEAsia_extent")


new_covs <- lapply(new_cov_files, raster)

new_covs <- raster::stack(new_covs)

old_covs <- brick("data/clean/raster/SEAsia_covs")

r_ext <- extent(blank)
random_x <- runif(10000, r_ext[1],r_ext[2])
random_y <- runif(10000, r_ext[3],r_ext[4])

points <- SpatialPointsDataFrame(coords=data.frame(long=random_x,lat=random_y),data=data.frame(id=1:length(random_x)))


data_old <- raster::extract(old_covs, points)
data_new <- raster::extract(new_covs, points)

data_old <- data_old[!is.na(data_old[,1]),]
data_new <- data_new[!is.na(data_new[,1]),]


df <- cbind(data_new,data_old)

colnames(df) <- c(new_cov_names,names(old_covs))

df <- as.data.frame(df)

plot(df, pch='.', cex=0.9, col=rgb(0,0,0,0.8))

saveRDS(df, "data/raw/covariate_production/cov_analysis_df.rds")

data_long <- df %>%
  mutate(pixel = row_number()) %>%
  pivot_longer(cols = -matches("pixel"))


saveRDS(data_long, "data/raw/covariate_production/cov_analysis_df_long.rds")

