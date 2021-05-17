
setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

# Map libraries
library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")

species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

risk_raster <- raster("output/update/rasters/buildup_4c_risk_mean.tif")

risk_df <- as.data.frame(risk_raster * species_extent, xy=TRUE)
colnames(risk_df) <- c("x", "y", "risk")

ggplot(risk_df) +
  geom_raster(aes(x=x, y=y, fill = risk)) +
  scale_fill_gradientn(colours=colors_risk,
                       limits=c(0,1),
                       na.value = 'white',
                       name = expression(italic("P. knowlesi")~' transmission suitability'),
                       breaks = c(0,1)) +
  
  geom_sf(data = SEA_simple,
          col=rgb(1,1,1,0.3),
          fill=NA,
          size = 0.1) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.8,0.7)) +
  guides(fill = guide_colorbar(label.position = 'bottom',
                               title.position = 'top',
                               ticks = FALSE,
                               barwidth = 12,
                               barheight = 0.7,
                               direction = 'horizontal'))


ggsave("output/figures/SEA_risk.pdf", width = 6, height = 4)
