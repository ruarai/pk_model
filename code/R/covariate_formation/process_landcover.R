
library(raster)
library(tidyverse)



blank <- raster("data/clean/raster/SEAsia_extent")
blank[!is.na(blank)] <- 1


landcover_files <- list.files("data/raw/covariates_MAP/landcover",
                              full.names = TRUE)

landcover_filenames <- list.files("data/raw/covariates_MAP/landcover",
                                  full.names = FALSE)

landcover <- tibble(files = landcover_files, filenames = landcover_filenames)

include_cov_names <- c("cropland_natural_vegetation_mosaic",
                       "grasslands",
                       "savannas",
                       "woody_savannas",
                       "open_shrublands",
                       "permanent_wetlands",
                       "cropland_natural_vegetation_mosaic",
                       "croplands",
                       "urban_and_built_up")

landcover <- landcover %>%
  filter(!str_detect(filenames, ".aux.xml")) %>%
  mutate(year = str_extract(filenames, "\\d{4}(?=\\.Annual\\.Data)")) %>%
  mutate(class_id = as.numeric(str_extract(filenames, "(?<=Class-)\\d{2}"))) %>%
  mutate(class_name = str_extract(filenames, "(?<=Class-\\d{2}_).+?(?=\\.)")) %>%
  mutate(class_name = str_to_lower(class_name)) %>%
  filter(class_name %in% include_cov_names) %>%
  mutate(band_name = glue::glue("{class_name}_{year}")) %>%
  rowwise() %>%
  mutate(raster = list(raster(files)))


landcover_raster <- landcover %>%
  mutate(non_unique = "") %>%
  group_by(non_unique) %>%
  summarise(full_band = list(stack(raster))) %>%
  pull(full_band) %>%
  pluck(1)

landcover_raster <- landcover_raster * blank

names(landcover_raster) <- landcover$band_name

writeRaster(landcover_raster,
            "data/raw/covariate_production/temporal_final/landcover_stack",
            format="raster",
            overwrite=TRUE)



