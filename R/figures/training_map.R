
# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")



data_all <- read.csv("data/clean/occurrence/data_all.csv")


occ_points <- data_all %>%
  filter(Geometry_type == 'point' & PA == 1)

abs_points <- data_all %>%
  filter(Geometry_type == 'point' & PA == 0)

poly_occ <- data_all %>%
  filter(Geometry_type == 'polygon')


cell_ns <- raster::cellFromXY(mbs_mask, poly_occ %>% select("Longitude", "Latitude"))
mbs_vals <- getValues(mbs_mask)
cell_table <- table(cell_ns)
mbs_vals[as.numeric(names(cell_table))] <- as.vector(cell_table)
mbs_overlaps <- setValues(mbs_mask, mbs_vals)



poly_occ <- poly_occ %>%
  distinct(Unique_ID, .keep_all = TRUE)

source("code/R/figures/admin_common.R")

adm1_occ <- get_polygons_1(poly_occ %>% select("Longitude", "Latitude"))
adm2_occ <- get_polygons_2(poly_occ %>% select("Longitude", "Latitude"))

mbs_coord <- coord_sf(xlim = extent(mbs_mask)[1:2],
                      ylim = extent(mbs_mask)[3:4],
                      expand=FALSE,
                      datum = NA)
ggplot() +
  geom_sf(data = SEA_simple,
          col='grey85',
          fill='grey85',
          size = 0.3) + 
  geom_raster(data = as.data.frame(mbs_overlaps,xy=TRUE),
              mapping = aes(x =x, y=y, fill = layer)) +
  
  mbs_coord +
  
  #scale_fill_distiller(palette = "Oranges",
  #                     na.value = NA,
  #                     direction = 1,
  #                     name = "Number of presence polygons over pixel",
  #                     breaks = 1:20,
  #                     labels = function(i) ifelse(i %% 5 == 0 | i == 1 | i == 18, i, ""),
  #                     rescaler = function(x, from) scales::rescale(x, from = from, to = c(0,0.7))) +
  
  scale_fill_stepsn(colours = c("#fff4e9", "#fb7849"),
                    breaks = 1:20,
                    name = "Number of presence polygons over pixel",
                    labels = function(i) ifelse(i %% 5 == 0 | i == 1 | i == 18, i, ""),
                    na.value = NA) +
  
  geom_point(data = abs_points,
             aes(x=Longitude, y = Latitude, color='a'),
             size = 0.05, stroke=0) +
  
  geom_point(data = occ_points,
             aes(x=Longitude, y = Latitude, color = 'b'),
             size = 0.8) +
  
  scale_color_manual(values = c('a' = "grey10",'b' = '#cc331a'),
                     labels = c("Absence", "Presence"),
                     name="Points") +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_colorsteps(label.position = 'bottom',
                                 title.position = 'top',
                                 barwidth = 12,
                                 barheight = 0.7,
                                 direction = 'horizontal'))

ggsave("output/figures/MBS_training_2.pdf", width = 8, height = 3)
