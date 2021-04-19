
library(raster)
library(tidyverse)


setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


temporal_inputs <- list.files("data/raw/covariate_production/temporal_final/",
                              pattern="*.grd",
                              full.names = TRUE)

nontemporal_inputs <- list.files("data/raw/covariate_production/nontemporal_final/",
                                 pattern="*.grd",
                                 full.names = TRUE)

temporal_bricks <- lapply(temporal_inputs, brick)
nontemporal_bricks <- lapply(nontemporal_inputs, brick)


temporal_stack <- stack(temporal_bricks)
nontemporal_stack <- stack(nontemporal_bricks)

temporal_meta <- tibble(bandname = names(temporal_stack)) %>%
  mutate(year = str_extract(bandname, "\\d{4}$"),
         varname = str_extract(bandname, "^.+(?=_)"))

data_range <- temporal_meta %>%
  group_by(varname) %>%
  summarise(max_year = max(year),
            min_year = min(year)) %>%
  summarise(min_max_year = min(max_year),
            max_min_year = max(min_year))

temporal_meta <- temporal_meta %>%
  mutate(include = year >= data_range$max_min_year &
                   year <= data_range$min_max_year)


layers_to_drop <- temporal_meta %>%
  filter(!include) %>%
  pull(bandname)

temporal_limited <- dropLayer(temporal_stack, layers_to_drop)



writeRaster(temporal_limited,
            "data/clean/raster_updated/temporal_SEA")

writeRaster(nontemporal_stack,
            "data/clean/raster_updated/nontemporal_SEA")




mbs_mask <- raster("data/clean/raster/mbs_mask")

temporal_limited_mbs <- temporal_limited * mbs_mask
nontemporal_stack_mbs <- nontemporal_stack * mbs_mask


writeRaster(temporal_limited_mbs,
            "data/clean/raster_updated/temporal_MBS")


writeRaster(nontemporal_stack_mbs,
            "data/clean/raster_updated/nontemporal_MBS")



latest_year_temporals <- names(temporal_limited)[str_detect(names(temporal_limited), data_range$min_max_year)]
non_latest_year <- setdiff(names(temporal_limited),latest_year_temporals)
latest_year_temporal <- dropLayer(temporal_limited, non_latest_year)

names(latest_year_temporal) <- str_replace(names(latest_year_temporal), 
                                           glue::glue("_{data_range$min_max_year}"),
                                           "")

prediction_raster <- stack(nontemporal_stack,
                           latest_year_temporal)



writeRaster(prediction_raster,
            "data/clean/raster_updated/prediction_SEA")





