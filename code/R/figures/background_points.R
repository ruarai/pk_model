# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

background_points <- read_csv("data/clean/occurrence/ALL_background_points.csv")




ggplot() +
  geom_sf(data = SEA_simple,
          fill= 'gray70',
          alpha = 0.1,
          color = rgb(0,0,0,0.3),
          size = 0.2) +
  
  geom_sf(data = MBS_simple,
          fill='white',
          size = 0.2) +
  
  geom_point(aes(x = Longitude, y = Latitude, color = 'b',),
             background_points %>% filter(Host == "human"),
             stroke = 0, size = 0.3,
             alpha = 0.25,
             position = position_jitter(width = 0.02,
                                        height = 0.02)) +
  
  geom_point(aes(x = Longitude, y = Latitude, shape = Host),
             background_points %>% filter(Host != "human")) +
  
  coord_sf(xlim = extent(mbs_mask)[1:2] + c(0, 0.1),
           ylim = extent(mbs_mask)[3:4] + c(-0.1, 0),
           expand=FALSE,
           datum = NA) +
  
  scale_color_manual("Points",
                     labels = c("b" = "Generated background points"),
                     values = c("b" = "#720e46")) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.5,0.7),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key = element_rect(color = 'black',
                                  size = 0.2,
                                  fill = 'white')) + 
  guides(colour = guide_legend(override.aes = list(size=0.25,
                                                   alpha = 1,
                                                   stroke = 1)))


aspect_ratio <- dim(mbs_mask)[2] / dim(mbs_mask)[1]
ggsave("output/figures/MBS_background_points.pdf", width =  9, height = 9 / aspect_ratio)
