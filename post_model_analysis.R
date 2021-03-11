
outpath <- 'output/update/'
load(paste0(outpath, "brt_model_list.RData"))

# prepare dummy prediction rasters for host, vector and monkey host species
mosquito_ras <- raster('data/clean/raster/mbs_mask.grd')
monkey_ras <- mosquito_ras
human_ras <- mosquito_ras
blank_ras <- mosquito_ras - 1

# add each layer to covs_current to make a covariate set for each species for model prediction
mosquito <- addLayer(covs_current, mosquito_ras, blank_ras, blank_ras)
names(mosquito)[names(mosquito)=='layer.1'] <- 'Host_mosquito'
names(mosquito)[names(mosquito)=='layer.2'] <- 'Host_monkey'
names(mosquito)[names(mosquito)=='layer.3'] <- 'Host_human'
monkey <- addLayer(covs_current, blank_ras, monkey_ras, blank_ras)
names(monkey)[names(monkey)=='layer.1'] <- 'Host_mosquito'
names(monkey)[names(monkey)=='layer.2'] <- 'Host_monkey'
names(monkey)[names(monkey)=='layer.3'] <- 'Host_human'
human <- addLayer(covs_current, blank_ras, blank_ras, human_ras)
names(human)[names(human)=='layer.1'] <- 'Host_mosquito'
names(human)[names(human)=='layer.2'] <- 'Host_monkey'
names(human)[names(human)=='layer.3'] <- 'Host_human'

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
  preds <- stack(preds_list)
  
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


# make prediction to rest of SE Asia, using human host raster

# get rasterbrick for SE Asia to predict to
seasia_covs <- brick('data/clean/raster/SEAsia_covs.grd')

# drop correlated layers
seasia_covs <- dropLayer(seasia_covs, c('EVI_mean', 'EVI_SD', 'TCB_mean'))

# prepare dummy prediction raster for human host
seasia_extent <- raster('data/clean/raster/SEAsia_extent.grd')
seasia_human_ras <- seasia_extent + 3
names(seasia_human_ras)[names(seasia_human_ras)=='layer'] <- 'Host_species'

# add human dummy raster to pred_covs
seasia_covs <- addLayer(seasia_covs, seasia_human_ras)

# initialize the cluster
sfInit(parallel = TRUE, cpus = ncpu)
sfLibrary(seegSDM)

# get predictions in parallel
preds_list_seasia <- sfLapply(model_list,
                              makePreds,
                              pred_covs = seasia_covs)

# summarise all the ensembles
preds_seasia <- stack(preds_list_seasia)

sfStop()

# summarise the predictions in parallel
preds_sry_seasia <- combinePreds(preds_seasia, parallel = FALSE)

names(preds_sry_seasia) <- c('mean',
                             'median',
                             'lowerCI',
                             'upperCI')


# save the prediction summary
writeRaster(preds_sry_seasia,
            file = paste0(outpath, 
                          'SEAsia'),
            format = 'GTiff',
            overwrite = TRUE)

# plot the risk map
png(paste0(outpath,
           'SEAsia_prediction_mean_human.png'),
    width = 2000,
    height = 2000,
    pointsize = 30)

par(oma = rep(0, 4),
    mar = c(0, 0, 0, 2))

plot(preds_sry_seasia[[1]],
     axes = FALSE,
     box = FALSE)

dev.off()


# ~~~~~~~~~~~~~~~
# plot effect curves

# plot the conditional effect curves
#   png(paste0(outpath,
#              'human_conditional_effects.png'),
#       width = 2000,
#       height = 2500, 
#       pointsize = 30)
#   
#  # par(mfrow = n2mfrow(length(model_list[[1]]$effects)))
#  
#  effects <- getConditionalEffectPlots(model_list,
#                                       plot = TRUE, hold = 14, value = 3)
#  
#  dev.off()

# # plot the marginal effect curves
# png(paste0(outpath,
#            'effects.png'),
#     width = 2000,
#     height = 2500,
#     pointsize = 30)
# 
# par(mfrow = n2mfrow(length(model_list[[1]]$effects)))
# 
# effects <- getEffectPlots(model_list,
#                           plot = TRUE)
# 
# dev.off()

# plot marginal effect curves for covariates where RI > 100/no. of covs
RI_threshold <- 100/nlayers(covs_current)

# find number of covs with RI > than threshold and get names
key_covs <- which(relinf[1:nlayers(prediction_covs)] > RI_threshold)
names <- rownames(relinf[1:length(key_covs),])

# get index of key covs
key_covs_idx <- which(names(prediction_covs) %in% names)

# create raster stack of covs with RI > threshold
key_covs_ras <- prediction_covs[[key_covs_idx]]

# get the order of plots (all except relinf)!
order <- match(names, names(key_covs_ras))

# set up x axis labels and titles (in order they appear in prediction_covs, enter manually)
short_names <- c('Human population',
                 'Elevation',
                 'Urban accessibility',
                 'Croplands',
                 'Macaca nemestrina')

units <- c('people',
           'metres',
           'time',
           'proportion',
           'suitability index') # check correct axis labels!!!

effect <- getEffectPlots(model_list, plot=FALSE)

# subset effect to only include those from key covs
key_effect <- effect[key_covs_idx] 

# set up device
png(paste0(outpath,
           'effects_figure.png'),
    width = 3000,
    height = 3000,
    pointsize = 60)

# set up multi panels and margins
par(mfrow = c(3,3),
    mar = c(5, 2, 4, 2),
    oma = c(0, 3, 0, 0))

# loop through plots
for (i in 1:length(key_covs)) {
  
  # extract summary stats
  df <- key_effect[[order[i]]][, 1:4]
  
  # pick y axis
  if (i %% 3 == 1) {
    ylab = 'marginal effect'
  } else {
    ylab = ''
  }
  
  # set up empty plotting region
  plot(df[, 2] ~ df[, 1],
       type = 'n',
       ylim = c(min(c(df[, 3], rev(df[, 4]))), max(c(df[, 3], rev(df[, 4])))+0.5),
       ylab = '',
       xlab = '')
  
  # add the 95% CIs
  polygon(x = c(df[, 1], rev(df[, 1])),
          y = c(df[, 3], rev(df[, 4])),
          border = NA,
          col = grey(0.7))
  
  # add the mean line
  lines(df[, 2] ~ df[, 1],
        lwd = 5,
        col = grey(0.2))
  
  # y axis lable (only on left hand column)
  title(ylab = ylab,
        cex.lab = 1.2,
        col.lab = grey(0.3),
        xpd = NA,
        line = 2.5)
  
  # x-axis label
  title(xlab = units[order[i]],
        cex.lab = 1.2,
        col.lab = grey(0.3),
        line = 2.5)
  
  # title
  title(main = short_names[order[i]],
        line = 1.5,
        cex.main = 1.2)
  
  # relative contribution inset
  mtext(text = round(relinf[i, 1] / 100, 2),
        side = 3,
        line = -1.8,
        adj = 0.07,
        col = grey(0.5))
  
  # get x values for data distribution plot
  #x_vals <- unique(dat_all[, which(colnames(data_list[[1]])==names[i])])
  #y_vals <- rep(1.4, length(x_vals))
  
  #points(x_vals, y_vals, cex=0.4, bg='black', pch=21)
  
}

dev.off()

# generate mess map
# drop host species layer
seasia_covs_mss <- dropLayer(seasia_covs, 'Host_species')

# calculate mess map
mss <- mess(seasia_covs_mss, dat_covs, full=TRUE)

# save rmess layer and the raster stack
writeRaster(mss[['rmess']], 
            file='output/mess_raw', 
            format='GTiff', 
            overwrite=TRUE)

writeRaster(mss, 
            file='output/mess_maps',
            overwrite=TRUE)

# make binary, interpolation/extrapolation map and save
tmp <- mss[['rmess']] >= 0

tmp_masked <- mask(tmp, seasia_extent)

writeRaster(tmp_masked, 
            file='output/mess_binary', 
            format='GTiff', 
            overwrite=TRUE)


# calculating standard deviation value for each pixel
# predict, type='response', which returns probabilities for bernoulli input data

# load required package
library(logit)

# to get back to log odds scale need to use logit function
preds_list_seasia_tf <- lapply(preds_list_seasia, logit)

# summarise all the ensembles
preds_seasia_tf <- stack(preds_list_seasia_tf)

# function to calculate the mean and standard deviation for each pixel across bootstraps
combine2 <- function(x){
  
  ans <- c(mean=mean(x), 
           sd=sd(x))
  
  return(ans)
}

preds_sry_tf <- calc(preds_seasia_tf, fun=combine2)

# save standard deviation plot
writeRaster(preds_sry_tf[[2]],
            file='output/sd_raster.tif',
            format='GTiff',
            overwrite=TRUE)

