
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


cite_packages <- c("tidyverse",
                   "ggplot2",
                   "seegSDM",
                   "raster",
                   "dismo",
                   "rgdal",
                   "rgeos",
                   "sf",
                   "sp",
                   "viridis",
                   "cowplot",
                   "gbm3",
                   "Matrix",
                   "sparseMatrixStats")


knitr::write_bib(x = cite_packages, file = "output/figures/package_refs.bib")
