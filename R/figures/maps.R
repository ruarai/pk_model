

library(raster)
library(tidyverse)
library(sf)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")



adm0_shape <- shapefile("data/raw/admin_maps/admin2013_0")

SEA_countries <- c('IND', 'MYS', 'IDN', 'LAO', 'MMR', 'PHL', 'THA', 'VNM', 'KHM', 'CHN', 'BGD', 'AUS', 'PNG')

SEA_shape <- adm0_shape[adm0_shape$COUNTRY_ID %in% SEA_countries,]

SEA_simple <- st_simplify(st_as_sf(SEA_shape), dTolerance = 0.01)




mean_SEA <- raster("output/update/rasters/buildup_4c_risk_mean.tif")
sd_SEA <- raster("output/update/rasters/buildup_4c_risk_sd.tif")


long_lats <- xyFromCell(mean_SEA, cell=1:ncell(mean_SEA))


raster_to_data <- function(raster_image){
  tibble(
    long = long_lats[,"x"],
    lat = long_lats[,"y"],
    mean = getValues(raster_image)
  )
}

cols <- colorRampPalette(c("#55843b", "#a4be79","#ffffbf", "#921d67", "#6c0043"))(100)

sea_overlay <-
  geom_sf(data = SEA_simple,
          col = rgb(1,1,1,alpha=0.4),
          fill=rgb(1,1,1,alpha=0),
          size = 0.3)
sea_coord <- 
  coord_sf(xlim = extent(mean_SEA)[1:2],
           ylim = extent(mean_SEA)[3:4],
           expand = FALSE,
           datum = NA)
  


p <- ggplot() +
  geom_raster(data = raster_to_data(mean_SEA),
              mapping = aes(x = long, y = lat, fill = mean)) +
  scale_fill_gradientn(colours = cols,
                       na.value = rgb(1,1,1,alpha=0),
                       limits=c(0,1),
                       breaks = c(0, 0.5, 1),
                       name = "Risk mean") +
  xlab("Longitude") + ylab("Latitude") +
  sea_overlay + sea_coord


ggsave("output/figures/risk_means.png", p)



p <- ggplot() +
  geom_raster(data = raster_to_data(sd_SEA),
              mapping = aes(x = long, y = lat, fill = mean)) +
  scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Risk SD") +
  xlab("Longitude") + ylab("Latitude") +
  sea_overlay + sea_coord

ggsave("output/figures/risk_sd.png", p)



pred_brick <- brick("data/clean/raster_updated/prediction_SEA")

pred_layers <- names(pred_brick)


for(pred_layer_i in pred_layers) {
  pred_raster <- pred_brick[[pred_layer_i]]
  
  
  
  p <- ggplot() +
    geom_raster(data = raster_to_data(pred_raster),
                mapping = aes(x = long, y = lat, fill = mean)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Value") +
    xlab("Longitude") + ylab("Latitude") +
    sea_overlay + sea_coord + 
    ggtitle(pred_layer_i)
  
  ggsave(paste0("output/figures/pred_", pred_layer_i, ".png"), p)
  
  p <- ggplot() +
    geom_raster(data = raster_to_data(pred_raster),
                mapping = aes(x = long, y = lat, fill = mean)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Value (log10)",
                       trans = "log10") +
    xlab("Longitude") + ylab("Latitude") +
    sea_overlay + sea_coord + 
    ggtitle(pred_layer_i)
  
  ggsave(paste0("output/figures/pred_", pred_layer_i, "_log10.png"), p)
}
















