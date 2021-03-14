
#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")
setwd("/data/gpfs/projects/punim1449/knowlesi_ruarai")

# Data manipulation
library(dplyr)
library(tidyr)

# Rasters
library(raster)

.libPaths()

# MI calculation
library(mpmi, lib.loc = "R_libs")


# Load the covariates from our raster
covs_current <- brick("covs_current.grd")


covs_data <- as.data.frame(values(covs_current)) %>%
  drop_na()

discrete_vars <- c("forest_intact",
                   "open_shrublands",
                   "woody_savannas",
                   "savannas",
                   "grasslands",
                   "permanent_wetlands",
                   "croplands",
                   "cropland_natural_vegetation_mosaic")

covs_continuous <- covs_data %>%
  dplyr::select(-all_of(discrete_vars))


start_time <- Sys.time()

covs_MI <- cmi(covs_continuous[,])

end_time <- Sys.time()

print(paste0("Completed in ", end_time - start_time))

write.csv(covs_MI,"out.csv")

