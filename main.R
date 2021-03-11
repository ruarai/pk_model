
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

set.seed(1)


ncpu <- 10
nboot <- ncpu*1


# Inclusive max/min
year_min <- 2001
year_max <- 2012


source('code_ruarai/functions_parasite.R')



source("code_ruarai/load_covariates.R")


source("code_ruarai/model_run.R")