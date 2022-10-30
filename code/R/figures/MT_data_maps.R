
try(pacman::p_unload("all"))
library(raster)
library(rgeos)
library(rgdal)
library(dismo)
library(png)
library(sf)
library(dplyr)

library(ggrepel)

MT_data <- read.csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv")

source("code/R/figures/maps_common.R")
source("code/R/figures/admin_common.R")

# Point data

points_data <- MT_data %>%
  filter(Geometry_type == 'point') %>%
  select(Longitude, Latitude, Host)

points_sp <- SpatialPointsDataFrame(points_data[,1:2],points_data %>% select(Host),
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

poly_data <- MT_data %>%
  filter(Geometry_type == 'polygon') %>%
  select(Longitude, Latitude, Admin_level, Host)


min_extent <- c(100, 120,-0,10)
highlight_extent <- c(108,120,-1,11)

library(grDevices)


extra_labels <- tribble(
  ~Longitude, ~Latitude, ~Label,
  5.82398, 95.305165, "(n = 15)",
  12.844231, 102.644998, "(n = 5)",
  1.605028, 111.673528, "(n = 12)",
  6.904875, 116.827899, "(n = 32)"
)



plot_for_host <- function(extent_for_host = NULL){
  if(poly_data %>% filter(Admin_level == '1') %>% nrow() > 0){
    poly_points_1 <- SpatialPoints(poly_data %>%
                                     filter(Admin_level == '1') %>% 
                                     select(Longitude, Latitude),
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    gids_1 <- over(poly_points_1, admin1_sea)$GID_1
    matches_1 <- admin1_sea[admin1_sea$GID_1 %in% gids_1,]
    shapes_1 <- st_simplify(st_as_sf(matches_1), dTolerance = 0.01)
  } else{
    shapes_1 <- NULL
  }
  
  poly_points_2 <- SpatialPoints(poly_data %>%
                                   filter(Admin_level == '2') %>% 
                                   select(Longitude, Latitude),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
  gids_2 <- over(poly_points_2, admin2_sea)$GID_2
  matches_2 <- admin2_sea[admin2_sea$GID_2 %in% gids_2,]
  shapes_2 <- st_simplify(st_as_sf(matches_2), dTolerance = 0.01)
  
  
  if(is.null(extent_for_host)){
    extent_for_host <- extent(shapes_2)
    extent_for_host <- raster::union(extent_for_host,
                                     extent(points_data %>%
                                              rename(x = Longitude, y = Latitude)))
    extent_for_host <- raster::union(extent_for_host,
                                     extent(min_extent))
  }
  
  
  labels_for_host <- extra_labels %>%
    bind_rows(points_data %>% mutate(Label = "")) # Append without labels to get text_repel to avoid other points
  
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
  
  p <- p +
    geom_sf(data = shapes_2[,],
            aes(fill = 'b'),
            colour = "white",
            size = 0.3) +
    
    geom_point(data = points_data,
               mapping = aes(x = Longitude, y = Latitude, color='point'),
               pch = 4, size = 1) +
    
    geom_text_repel(data = labels_for_host,
                    mapping = aes(y=Longitude, x = Latitude, label = Label),
                    box.padding = 2,
                    point.padding=0.5,
                    nudge_y = 0.5,
                    nudge_x = -2) +
    
    coord_sf(xlim = extent_for_host[1:2],
             ylim = extent_for_host[3:4],
             expand=TRUE,
             datum = NA) +
    
    scale_fill_manual(values = c('a' = "#d79da0",'b' = '#c7777b'),
                      labels = c("Admin 1 region", "Admin 2 region"),
                      name=NULL) +
    
    scale_color_manual(values = c('point' = 'black'),
                       labels = c('Points'),
                       name=NULL) +
    
    theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = c(0.75,0.8),
          legend.background = element_rect(fill = 'grey98'))
  
  p
}

p_map <- plot_for_host() +
  ggtitle("B - New infection occurrence records")

p_map

mt_data <- read_csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv") %>%
  distinct(ID, .keep_all = TRUE)



fs_data_mbs <- read_csv("data/raw/occurrence/polygon_data_mbs.csv") %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(Presence == 1)


fs_data_non_mbs <- read_csv("data/raw/occurrence/presence_absence_ex-MSB_confirmed.csv") %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(Presence == 1)


all_data_by_year <- bind_rows(
  mt_data %>% select(Year) %>% mutate(Source = "MT"),
  fs_data_mbs %>% select(Year) %>% mutate(Source = "FS"),
  fs_data_non_mbs %>% select(Year) %>% mutate(Source = "FS")
) %>%
  count(Source, Year) 



all_data_by_year$Source <- factor(all_data_by_year$Source, c("MT","FS"))

p_epi <- ggplot(all_data_by_year) +
  geom_col(aes(x=Year, y = n, fill = Source), position='stack') +
  scale_fill_manual(values = c("MT" = "#c7777b", "FS" = "#5d798c"),
                    labels = c("FS" = "Prior database (Shearer et al.)",
                               "MT" =  "Current study"),
                    guide = guide_legend(reverse = TRUE),
                    name = NULL) +
  xlab("Year") + ylab("Count") +
  scale_x_continuous(breaks = seq(0,10000,by=2),
                     minor_breaks = 1:10000) +
  theme(legend.position = c(0.25, 0.7),
        legend.background = element_rect(fill = 'grey98'),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = 'gray95'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = 'gray80')) +
  expand_limits(x = 2020) +
  
  ggtitle("C - Infection occurrence records by year")


cowplot::plot_grid(
  p_map, p_epi,
  ncol = 1, rel_heights = c(2.5, 1)
)


ggsave("output/figures/newdata.pdf",
       width = 9 * 0.65, height = 11 * 0.65,
       bg = "white")









