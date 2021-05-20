
library(raster)
library(dplyr)
library(stringr)

lc_files <- data.frame(file_path = list.files(path = "data/raw/covariate_production/MCD12Q1_rasters_original",
                                              pattern = "LC",
                                              full.names = TRUE))


lc_files$year <- str_extract(lc_files$file_path,"(?<=doy)(.*)(?=001_)")

lc_files$new_name <- str_c("MCD12Q1_", lc_files$year)

# Load in our raster files
lc_files$raster <- lapply(lc_files$file_path, raster)


sea_grid <- raster('data/clean/raster/SEAsia_extent.grd')

# Downsample onto our 5x5km reference grid
lc_files$raster_downsampled <- lapply(lc_files$raster, function(r){
  fact <- round(dim(r)[1:2] / dim(sea_grid)[1:2])
  
  
  
  open_shrublands <- aggregate(r, fact,
                               fun=function(x, na.rm=T) {sum(x==7, na.rm=na.rm)})
  
  
  
  x <- resample(open_shrublands, sea_grid)
  
  return(x)
})
