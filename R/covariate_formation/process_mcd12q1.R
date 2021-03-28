
library(raster)
library(dplyr)
library(stringr)

lc_files <- data.frame(file_path = list.files(path = "data/raw/covariate_production/MCD12Q1_rasters_original",
                                              pattern = "LC",
                                              full.names = TRUE))


lc_files$year <- str_extract(lc_files$file_path,"(?<=doy)(.*)(?=001_)")

lc_files$new_name <- str_c("MCD12Q1_", lc_files$year)

lc_files$raster <- lapply(lc_files$file_path, raster)




