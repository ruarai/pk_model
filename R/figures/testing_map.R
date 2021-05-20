# Map libraries
library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")

test_points <- read.csv("data/clean/occurrence/testing/test_points.csv")
stopifnot(all(test_points$PA == 1))


test_poly_data <- read.csv("data/clean/occurrence/testing/test_poly.csv")
test_poly_data$PA <- factor(test_poly_data$PA)

test_poly_shape <- readRDS("data/clean/occurrence/testing/poly_shapes.Rds")
rownames(test_poly_data) <- names(test_poly_shape)

test_poly_sdf <- SpatialPolygonsDataFrame(test_poly_shape, test_poly_data)



ggplot() +
  geom_sf(data = SEA_simple,
          col='grey80',
          size = 0.2) +
  
  geom_sf(data = MBS_simple,
          fill = 'grey40',
          col = rgb(0,0,0,0)) +
  
  geom_sf(data = st_as_sf(test_poly_sdf),
          aes(fill = PA),
          color=rgb(1,1,1,0.3),
          size = 0.2) +
  
  geom_blank(data = tibble(Nodata = "2"),
             mapping = aes(fill = Nodata)) +
  
  scale_fill_manual(values = c("0" = "#a0bd77", "1" = "#E1815D", "2" = "grey90"),
                    labels = c("Absence polygons", "Presence polygons", "No data"),
                    name = NULL) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  geom_point(data = test_points,
             aes(x = Longitude, y = Latitude, color = 'A'),
             size = 0.8) +
  
  scale_color_manual(values = c("A" = "#cc331a"),
                     labels = "Presence points",
                     name = NULL) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.8,0.7),
        legend.direction = "vertical")
  


ggsave("output/figures/SEA_testing.pdf", width = 9, height = 6)
