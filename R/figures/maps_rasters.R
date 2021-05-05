

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



library(viridis)

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

occ_data <- read.csv("data/clean/occurrence/pk_present/ALL_occ_thinned.csv")

df_points <- occ_data %>%
  filter(Geometry_type == "point") %>%
  bind_cols(.,
            raster::extract(pred_brick, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(c(all_of(pred_layers), Unique_ID)) %>%
  pivot_longer(cols = all_of(pred_layers)) %>%
  mutate(source = "points",
         type = "presence")
poly_points <- occ_data %>%
  filter(Geometry_type == "polygon") %>%
  group_by(Unique_ID) %>%
  sample_n(50, replace=TRUE) %>%
  ungroup() %>%
  bind_cols(.,
            raster::extract(pred_brick, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(c(all_of(pred_layers), Unique_ID)) %>%
  pivot_longer(cols = all_of(pred_layers)) %>%
  mutate(source = "polygons",
         type = "presence")

occ_pred <- bind_rows(poly_points, df_points) %>%
  rename(mean = value)


for(pred_layer_i in pred_layers) {
  pred_raster <- pred_brick[[pred_layer_i]]
  
  data <- raster_to_data(pred_raster)
  
  p1 <- ggplot() +
    geom_raster(data = data,
                mapping = aes(x = long, y = lat, fill = mean)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Value") +
    xlab("Longitude") + ylab("Latitude") +
    sea_overlay + sea_coord + 
    ggtitle(pred_layer_i)
  
  p2 <- ggplot() +
    geom_raster(data = data,
                mapping = aes(x = long, y = lat, fill = mean)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Value (log1p)",
                       trans = "log1p") +
    xlab("Longitude") + ylab("Latitude") +
    sea_overlay + sea_coord + 
    ggtitle(pred_layer_i)
  
  
  data_w_occ <- bind_rows(data %>%
                            mutate(type = "background"),
                          occ_pred %>% filter(name == pred_layer_i))
  
  p3 <- ggplot(data_w_occ, aes(x = mean)) +
    geom_histogram(fill = "#98bfd9") + 
    
    facet_grid(rows = vars(type), 
               scales="free",
               labeller = as_labeller(c("background" = "Values for all SEA region",
                                        "presence" = "Values for presence samples"))) +
    xlab("Value") + ylab("Density") +
    
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'gray90'),
          panel.grid.minor.x = element_line(colour = 'gray95'),
          axis.ticks.x = element_line(color = 'gray80'))
  
  p4 <- ggplot(data_w_occ, aes(x = mean)) +
    geom_histogram(fill = "#98bfd9") + 
    
    facet_grid(rows = vars(type), 
               scales="free",
               labeller = as_labeller(c("background" = "All SEA region",
                                        "presence" = "Sample points"))) +
    xlab("Value (Excluding 0, log10 transformed)") + ylab("Density") +
    
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'gray90'),
          panel.grid.minor.x = element_line(colour = 'gray95'),
          axis.ticks.x = element_line(color = 'gray80')) +
    
    scale_x_continuous(trans = "log10") + annotation_logticks(colour = "gray60", sides="b")
  
  plot_all <- plot_grid(p1,p2,p3,p4, nrow=2)
  ggsave(paste0("output/figures/raster_supp/",pred_layer_i,".pdf"), plot_all, width=8.75, height = 8)
}
















