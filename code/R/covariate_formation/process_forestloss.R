
library(raster)
library(tidyverse)


lossyear_raster <- brick("data/raw/covariate_production/global_forest_change/lossyear_downscale.tif")


blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1


lossyear_raster <- lossyear_raster * blank

names(lossyear_raster) <- c("forestloss_noloss", str_c("forestloss_", 2000+1:19), "forestloss_blank")

lossyear_raster <- dropLayer(lossyear_raster, c("forestloss_noloss",
                                                "forestloss_blank")) # unused layers


lookback_period_length <- 1

if(lookback_period_length > 1) {
  
  for(i_endyear in 19:lookback_period_length) {
    lookback_period <- (i_endyear - lookback_period_length + 1):(i_endyear)
    lookback_years <- str_c("forestloss_20", str_pad(lookback_period, width = 2, pad = '0'))
    
    summed_loss <- sum(lossyear_raster[[lookback_years]])
    
    lossyear_raster[[last(lookback_years)]] <- summed_loss
    
  }
  
  # Fill in the years for which we don't have a full lookback_period_length's worth of data for
  # with the data from the year for which we do
  start_years <- str_c("forestloss_200", 1:(lookback_period_length - 1))
  for(i_year in start_years){
    lossyear_raster[[i_year]] <- lossyear_raster[[str_c("forestloss_200", lookback_period_length)]]
  }
}





writeRaster(lossyear_raster,
            "data/raw/covariate_production/temporal_final/lossyear_stack",
            method = "raster",
            overwrite=TRUE)



treecover_raster <-  brick("data/raw/covariate_production/global_forest_change/treecover_downscale.tif")

treecover_raster <- treecover_raster * blank

names(treecover_raster) <- c(str_c("treecover_", 2000+0:19), "treecover_blank")

treecover_raster <- dropLayer(treecover_raster, c("treecover_blank"))

writeRaster(treecover_raster,
            "data/raw/covariate_production/temporal_final/treecover_stack",
            method = "raster",
            overwrite=TRUE)

