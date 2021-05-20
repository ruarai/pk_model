# AUC hack

getStats2 <- 
  function (object,
            test,
            cv = TRUE,
            pwd = TRUE,
            threshold = 1,
            ...) {
    
    
    #object <- model_list[[1]]
    
    #test <- val_dat
    
    # get test x data
    x.test.data <- test[,4:ncol(test)]
    
    # get test y data
    y.test.data <- test$Presence
    
    # get observed data from training region
    y.data <- object$model$data$y
    
    # squish covariates back into a matrix
    x.data <- matrix(object$model$data$x,
                     nrow = length(y.data))
    
    # then coerce to a dataframe
    x.data <- data.frame(x.data)
    
    # and give them back their names
    names(x.data) <- colnames(object$model$data$x.order)
    
    
    # if pair-wise distance sampling is required
    if (pwd) {
      
      # get model
      model <- object$model
    
      # predicted probabilities to test set
      pred <- predict.gbm(model,
                          x.test.data,
                          n.trees = model$n.trees,
                          type = 'response')
        
      # training presence point index
      train_p <- which(y.data == 1)
        
      # test presence point index
      test_p <- which(y.test.data == 1)
        
      # test absence point index
      test_a <- which(y.test.data == 0)
        
      x <- pwdSample(test[test_p, c('Longitude', 'Latitude')],
                     test[test_a, c('Longitude', 'Latitude')],
                     object$coords[train_p, ],
                     n = 1, tr = 1)
        
      keep_p <- which(!is.na(x[, 1]))
      keep_a <- na.omit(x[, 1])
        
      keep <- c(test_p[keep_p], test_a[keep_a])
        
      # handle the case that pwdSample returns NAs
      if (length(keep) == 0) {
          
      # if so, return NAs too
      preds <- data.frame(PA = rep(NA, 3),
                          pred = rep(NA, 3))
          
      # and issue a warning
      warning (paste0('failed to carry out pwd sampling in submodel ',
                      i))
          
      } else {
          
      #add an evaluation dataframe to list
      preds <- data.frame(PA = y.test.data[keep],
                          pred = pred[keep])
      
      }
        
      } else {
        
        # get model
        model <- object$model
        
        # predicted probabilities to test set
        pred <- predict.gbm(model,
                            x.test.data,
                            n.trees = model$n.trees,
                            type = 'response')
        
        preds <- data.frame(PA = y.test.data,
                            pred = pred)
        
      }
      
      # calculate cv statistics for all folds
      stats <- calcStats(preds)
      
      # return the mean of these
      #return (colMeans(stats, na.rm = TRUE))
      
      return(stats)
    
} # close pwd = FALSE, cv else
      

# Calculate range of validation statistics for fitted models.
# df is a dat.frame or matrix containing observed 0 or 1 data in
# the first column and (0, 1] predictions in the second
calcStats <- function(df) {
  
  # if any elements of df are NAs, return NAs
  if (any(is.na(df))) {
    
    result <- auc = NA
    
  } else {
    
    # add an id column (needed for PresenceAbsence functions)
    df <- data.frame(id = 1:nrow(df), df)
    
    # ~~~~~~~~~~
    # auc (using my safe version - see above)
    auc <- auc2(df, st.dev = TRUE)
    
    # create results vector
    result <- c(auc = auc[, 1],
                auc_sd = auc[, 2])
  
  }
  
  # and return it
  return (result)
}

# clone of the auc function in PresenceAbsence
# but without the shocking
auc2 <- function (DATA,
                  st.dev = TRUE,
                  which.model = 1,
                  na.rm = FALSE) {
  if (is.logical(st.dev) == FALSE) {
    stop ("'st.dev' must be of logical type")
  }
  if (is.logical(na.rm) == FALSE) {
    stop ("'na.rm' must be of logical type")
  }
  if (sum(is.na(DATA)) > 0) {
    if (na.rm == TRUE) {
      NA.rows <- apply(is.na(DATA), 1, sum)
      warning (length(NA.rows[NA.rows > 0]), " rows ignored due to NA values")
      DATA <- DATA[NA.rows == 0, ]
    } else {
      return (NA)
    }
  }
  if (length(which.model) != 1) {
    stop ("this function will only work for a single model, 'which.model' must be of length one")
  }
  if (which.model < 1 || round(which.model) != which.model) {
    stop ("'which.model' must be a positive integer")
  }
  if (which.model + 2 > ncol(DATA)) {
    stop ("'which.model' must not be greater than number of models in DATA")
  }
  DATA <- DATA[, c(1, 2, which.model + 2)]
  DATA[DATA[, 2] > 0, 2] <- 1 # make value of presences equal 1
  OBS <- DATA[, 2] # presence/absence
  PRED <- DATA[, 3] # predictions
  if (length(OBS[OBS == 1]) == 0 || length(OBS[OBS == 1]) == 
      nrow(DATA)) {
    if (st.dev == FALSE) {
      return (NaN)
    } else {
      return (data.frame(AUC = NaN, AUC.sd = NaN))
    }
  }
  rm(DATA)
  PRED.0 <- PRED[OBS == 0] # predictions for absences
  PRED.1 <- PRED[OBS == 1] # predictions for presences
  N <- length(PRED)
  n0 <- as.double(length(PRED.0))
  n1 <- as.double(length(PRED.1))
  R <- rank(PRED, ties.method = "average") # rank predictions from lowest to highest???
  R0 <- R[OBS == 0] # 
  R1 <- R[OBS == 1]
  U <- n0 * n1 + (n0 * (n0 + 1)) / 2 - sum(R0)
  AUC <- U / (n0 * n1)
  

  rm(PRED)
  rm(OBS)
  if (st.dev == FALSE) {
    return (AUC = AUC)
  } else {
    RR0 <- rank(PRED.0, ties.method = "average") 
    RR1 <- rank(PRED.1, ties.method = "average")
    pless.0 <- (R0 - RR0) / n1
    pless.1 <- (R1 - RR1) / n0
    var.0 <- var(pless.0)
    var.1 <- var(pless.1)
    var.AUC <- (var.0 / n0) + (var.1 / n1)
    st.dev.AUC <- var.AUC ^ 0.5
    return (data.frame(AUC = AUC, AUC.sd = st.dev.AUC))
  }
}


# load validation data set
outside_MBS_occ <- read.csv('data/raw/occurrence/presence_absence_ex-MSB_confirmed.csv')

# load master mask for seasia covs
seasia_mask <- raster('data/clean/raster/SEAsia_extent.grd')

# find index of all points falling outside the master mask
outside_idx <- which(is.na(extract(seasia_mask, outside_MBS_occ[,c('Longitude', 'Latitude')])))

# remove these points
outside_MBS_occ <- outside_MBS_occ[-outside_idx,]

# get covariate values 
validation_dat_covs <- extract(seasia_covs, outside_MBS_occ[, c('Longitude', 'Latitude')])

# combine with lats/longs/etc. 
val_dat <- cbind(outside_MBS_occ[,c('Presence','Longitude', 'Latitude')], validation_dat_covs)

# get validation statistics in parallel
stat_lis_val <- lapply(model_list, getStats2, test=val_dat, pwd = FALSE)

# convert the stats list into a matrix using the do.call function
stats_val <- do.call("rbind", stat_lis_val)

# save them
write.csv(stats_val,
          paste0(outpath,
                 'stats_val.csv'))



