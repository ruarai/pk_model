
outpath <- 'output/update/'
load(paste0(outpath, "brt_model_list.RData"))

covs_current <- brick("data/clean/raster/covs_current.grd")

mbs_land <- raster('data/clean/raster/mbs_mask.grd')
blank_raster <- mbs_land - 1

rename_layers_model_matrix <- function(raster){
  names(raster)[names(raster)=='layer.1'] <- 'Host_mosquito'
  names(raster)[names(raster)=='layer.2'] <- 'Host_monkey'
  names(raster)[names(raster)=='layer.3'] <- 'Host_human'
  
  return(raster)
}

# add each layer to covs_current to make a covariate set for each species for model prediction
mosquito <- addLayer(covs_current, mbs_land, blank_raster, blank_raster)
mosquito <- rename_layers_model_matrix(mosquito)

monkey <- addLayer(covs_current, blank_raster, mbs_land, blank_raster)
monkey <- rename_layers_model_matrix(monkey)

human <- addLayer(covs_current, blank_raster, blank_raster, mbs_land)
human <- rename_layers_model_matrix(human)



# ~~~~~~~~~~~~~~
# make predictions

# loop through each set of covariates, making model predictions and plotting  
for (host in c('human','monkey', 'mosquito')){
  
  # get rasterbrick for these covs
  prediction_covs <- get(host)
  
  # get predictions in parallel
  preds_list <- lapply(model_list,
                         function(m) {predict(prediction_covs, m, type="response", n.trees = length(m$trees))})
  
  # summarise all the ensembles
  preds <- stack(preds_list) # Possibly OK to use quick = TRUE
  
  # summarise the predictions 
  preds_sry <- combinePreds(preds, parallel = FALSE)
  
  names(preds_sry) <- c('mean',
                        'median',
                        'lowerCI',
                        'upperCI')
  
  # save the prediction summary
  writeRaster(preds_sry,
              file = paste0(outpath, 
                            'parasite_', 
                            host),
              format = 'GTiff',
              overwrite = TRUE)
  
  # plot the risk map
  png(paste0(outpath,
             host,
             '_prediction_mean.png'),
      width = 2000,
      height = 2000,
      pointsize = 30)
  
  par(oma = rep(0, 4),
      mar = c(0, 0, 0, 2))
  
  plot(preds_sry[[1]],
       axes = FALSE,
       box = FALSE)
  
  dev.off()
  
}


