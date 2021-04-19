

library(tidyverse)

library(raster)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")



process_oneoffs <- function(dirpath, varname){
  raster_files <- list.files(dirpath,
                             full.names = TRUE)
  
  stack_with_names <- function(rasters, var_names){
    stacked <- stack(rasters)
    
    names(stacked) <- var_names
    
    return(stacked)
  }
  
  raster_meta <- tibble(filepath = raster_files) %>%
    mutate(year = str_extract(filepath, "\\d{4}(?=\\.Annual)")) %>%
    mutate(bandname = glue::glue("{varname}_{year}"))
  
  print(raster_meta)
  
  raster_stacked <- raster_meta %>%
    rowwise() %>%
    mutate(raster = list(raster(filepath))) %>%
    ungroup %>%
    mutate(nonunique = "") %>%
    group_by(nonunique) %>%
    summarise(full_raster = list(stack_with_names(raster, bandname))) %>%
    pull(full_raster) %>%
    pluck(1)
  
  writeRaster(raster_stacked, glue::glue("data/raw/covariates_MAP/{varname}_stack"), format="raster")  
}
process_oneoffs("data/raw/covariates_MAP/WorldPop/",
                "human_pop")

process_oneoffs("data/raw/covariates_MAP/TCW/",
                "TCW_mean")

