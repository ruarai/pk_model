
library(dplyr)

print("Starting feature optimizer.")

pred_raster <- brick("data/clean/raster_updated/prediction_SEA")
features_all <- names(pred_raster)

features_best <- features_all


# Calculate CV from all features
cv_best <- 0.5

attempted_feature_sets <- list()


for(i in 1:100){
  features_i <- NULL
  while(is.null(features_i)) {
    removal_candidate <- sample(features_best, size = 1)
    
    features_i <- setdiff(features_best, removal_candidate)
    
    # Need to keep resampling until we come up with a unique feature set
    if(list(features_i) %in% attempted_feature_sets){
      features_i <- NULL
    }
  }
  
  attempted_feature_sets <- c(attempted_feature_sets,
                              list(features_i))
  
  
  cv_i <- runif(1)
  
  
  if(cv_i > cv_best){
    features_best <- features_i
    cv_best <- cv_i
  } else {
    
  }
}

