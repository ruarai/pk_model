



try(pacman::p_unload("all"))

library(raster)
library(sf)
library(tidyverse)


source("code/R/figures/maps_common.R")
source("code/R/figures/admin_common.R")


map_lims <- extent(sea_mask)


all_occ <- read_csv("data/clean/occurrence/pk_present/ALL_occ_thinned.csv")



ggplot() +
  geom_sf(data = SEA_simple,
          col='grey80',
          size = 0.3) +
  
  coord_sf(xlim = map_lims[1:2],
           ylim = map_lims[3:4],
           expand=TRUE,
           datum = NA) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")





