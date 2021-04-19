

library(tidyverse)

library(raster)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1

process_oneoffs <- function(dirpath, varname){
  raster_files <- list.files(dirpath,
                             full.names = TRUE)
  
  
  raster_meta <- tibble(filepath = raster_files) %>%
    mutate(year = str_extract(filepath, "\\d{4}(?=\\.Annual)")) %>%
    mutate(bandname = glue::glue("{varname}_{year}"))
  
  print(raster_meta)
  
  raster_stacked <- raster_meta %>%
    rowwise() %>%
    mutate(raster = list(raster(filepath))) %>%
    mutate(raster = list(raster * blank)) %>%
    ungroup %>%
    mutate(nonunique = "") %>%
    group_by(nonunique) %>%
    summarise(full_raster = list(stack(raster))) %>%
    pull(full_raster) %>%
    pluck(1)
  
  names(raster_stacked) <- raster_meta$bandname
  
  raster_stacked <- raster_stacked * blank
  
  writeRaster(raster_stacked, glue::glue("data/raw/covariate_production/temporal_final/{varname}_stack"), format="raster")  
}
process_oneoffs("data/raw/covariates_MAP/WorldPop/",
                "human_pop")

process_oneoffs("data/raw/covariates_MAP/TCW/",
                "TCW_mean")

process_oneoffs("data/raw/covariates_MAP/TCW_SD/",
                "TCW_SD")

process_oneoffs("data/raw/covariates_MAP/TCB_SD/",
                "TCB_SD")
