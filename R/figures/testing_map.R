
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

# Map libraries
library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")
source("code_ruarai/R/figures/admin_common.R")



test_poly_2 <- test_poly %>%
  filter(Admin_level == 2) %>%
  select(Longitude, Latitude) %>%
  get_polygons_2()

test_poly_1 <- test_poly %>%
  filter(Admin_level == 1) %>%
  select(Longitude, Latitude) %>%
  get_polygons_1()

test_poly_shape <- do.call(rbind, list(test_poly_1, test_poly_2))



ggplot() +
  geom_sf(data = SEA_simple,
          col='grey80',
          size = 0.3) +
  
  geom_sf(data = MBS_simple,
          fill = 'grey20',
          col = rgb(0,0,0,0)) +
  
  geom_sf(data = st_as_sf(poly_shapes),
          fill = 'red') +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  geom_point(data = test_data,
             aes(x = Longitude, y = Latitude),
             size = 1) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
  
