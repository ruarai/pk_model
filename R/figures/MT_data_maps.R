
library(raster)
library(rgeos)
library(rgdal)
library(dismo)
library(png)
library(sf)
library(dplyr)
library(tmap)

library(ggrepel)

#setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

MT_data <- read.csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv")

adm0_shape <- shapefile("data/raw/admin_maps/admin2013_0")

SEA_countries <- c('IND', 'MYS', 'IDN', 'LAO', 'MMR', 'PHL', 'THA', 'VNM', 'KHM', 'CHN', 'BGD')

SEA_shape <- adm0_shape[adm0_shape$COUNTRY_ID %in% SEA_countries,]

SEA_simple <- st_simplify(st_as_sf(SEA_shape), dTolerance = 0.01)


admin1_shp <- shapefile('data/raw/gadm_maps/gadm36_1.shp')
admin2_shp <- shapefile('data/raw/gadm_maps/gadm36_2.shp')

admin1_sea <- admin1_shp[admin1_shp$GID_0 %in% SEA_countries,]
admin2_sea <- admin2_shp[admin2_shp$GID_0 %in% SEA_countries,]

# Point data

points_data <- MT_data %>%
  filter(Geometry_type == 'point') %>%
  select(Longitude, Latitude, Host)

points_sp <- SpatialPointsDataFrame(points_data[,1:2],points_data %>% select(Host),
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

poly_data <- MT_data %>%
  filter(Geometry_type == 'polygon') %>%
  select(Longitude, Latitude, Admin_level, Host)


min_extent <- c(100, 120,-10,10)
highlight_extent <- c(108,120,-1,11)

library(grDevices)

facet_names <- c("human" = "Human",
                 "monkey" = "Macaque",
                 "mosquito" = "Mosquito")


extra_labels <- tribble(
  ~Longitude, ~Latitude,  ~Host, ~Label,
  5.82398, 95.305165, "human", "(n = 15)",
  12.844231, 102.644998, "human", "(n = 5)",
  1.605028, 111.673528, "human", "(n = 12)",
  6.904875, 116.827899, "human", "(n = 29)",
  7.17605, 117.06136, "mosquito", "(n = 3)",
)


plot_for_host <- function(host, extent_for_host = NULL, draw_region = FALSE){
  if(poly_data %>% filter(Admin_level == '1', Host == host) %>% nrow() > 0){
    poly_points_1 <- SpatialPoints(poly_data %>%
                                     filter(Admin_level == '1', Host == host) %>% 
                                     select(Longitude, Latitude),
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    gids_1 <- over(poly_points_1, admin1_sea)$GID_1
    matches_1 <- admin1_sea[admin1_sea$GID_1 %in% gids_1,]
    shapes_1 <- st_simplify(st_as_sf(matches_1), dTolerance = 0.01)
  } else{
    shapes_1 <- NULL
  }
  
  poly_points_2 <- SpatialPoints(poly_data %>%
                                   filter(Admin_level == '2', Host == host) %>% 
                                   select(Longitude, Latitude),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
  gids_2 <- over(poly_points_2, admin2_sea)$GID_2
  matches_2 <- admin2_sea[admin2_sea$GID_2 %in% gids_2,]
  shapes_2 <- st_simplify(st_as_sf(matches_2), dTolerance = 0.01)
  
  
  if(is.null(extent_for_host)){
    extent_for_host <- extent(shapes_2)
    extent_for_host <- raster::union(extent_for_host,
                                     extent(points_data %>%
                                              filter(Host == host) %>%
                                              rename(x = Longitude, y = Latitude)))
    extent_for_host <- raster::union(extent_for_host,
                                     extent(min_extent))
  }
  
  
  labels_for_host <- extra_labels %>%
    filter(Host == host &
             extent_for_host[1] < Latitude & Latitude < extent_for_host[2] &
             extent_for_host[3] < Longitude & Longitude < extent_for_host[4])
  
  p <- ggplot() +
    geom_sf(data = SEA_simple,
            col='grey80',
            size = 0.3) 
  if(!is.null(shapes_1)){
    p <- p +
      geom_sf(data = shapes_1[,],
              aes(fill = 'a'),
              colour = "white", 
              size = 0.3)
  }
  
  if(draw_region){
    p <- p +
      geom_rect(aes(xmin=highlight_extent[1], xmax=highlight_extent[2],
                    ymin=highlight_extent[3], ymax=highlight_extent[4]),
                fill=NA,
                colour='gray60')
  }
  
  
  p <- p +
    geom_sf(data = shapes_2[,],
            aes(fill = 'b'),
            colour = "white",
            size = 0.3) +
    
    geom_point(data = points_data %>% filter(Host == host),
               mapping = aes(x = Longitude, y = Latitude, color='point'),
               shape=20, size = 1) +
    
    geom_text_repel(data = labels_for_host,
                    mapping = aes(y=Longitude, x = Latitude, label = Label),
                    box.padding = 1.5,
                    point.padding=0.2,
                    nudge_y=0.5, nudge_x = -0.2) +
    
    coord_sf(xlim = extent_for_host[1:2],
             ylim = extent_for_host[3:4],
             expand=TRUE,
             datum = NA) +
    
    scale_fill_manual(values = c('a' = "#7191af",'b' = '#23598b'),
                      labels = c("Admin 1", "Admin 2"),
                      name="Polygons") +
    
    scale_color_manual(values = c('point' = rgb(0.8,0.2,0.1)),
                       labels = c(''),
                       name="Points") +
    
    theme(panel.background = element_rect(fill='white'),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "none")
  
  p
}

p1 <- plot_for_host("human", draw_region = TRUE) +
  ggtitle("Human")

p2 <- plot_for_host("human", highlight_extent) +
  ggtitle("Human - East Malaysia")

p3 <- plot_for_host("mosquito") +
  ggtitle("Mosquito")

p4 <- plot_for_host("monkey") +
  ggtitle("Macaque")


library(cowplot)

p_full <- plot_grid(p1, p2, p3, p4,
                    nrow = 1,
                    align='h')

plot_legend <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(p_full, plot_legend, ncol = 1, rel_heights = c(1, .2))

ggsave("output/figures/MT_data_maps.pdf",
       width = 12, height=4)



