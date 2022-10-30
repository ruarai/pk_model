

# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

data_all <- read.csv("data/clean/occurrence/data_all.csv")

host_col <- which(colnames(data_all) == "Host_species")

data_covs <- data_all %>%
  select((host_col+1):last_col())


pred_raster <- brick("data/clean/raster_updated/prediction_SEA")



mess_raster <- dismo::mess(pred_raster, data_covs, full = TRUE)


mess_binary <- (mess_raster[['rmess']] >= 0) * (sea_mask)
mess_df <- as.data.frame(mess_binary, xy=TRUE)
mess_df$layer <- factor(mess_df$layer)

mess_df %>% write_rds("output/update/mess_df.rds")


aspect_ratio <- dim(pred_raster)[1] / dim(pred_raster)[2] 
ggsave("output/figures/SEA_mess.png",
       width = 9, height = 9 * aspect_ratio)
