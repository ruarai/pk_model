

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

# Map libraries
library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")



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

source("code_ruarai/R/figures/admin_common.R")

adm1_occ <- get_polygons_1(poly_occ %>% select("Longitude", "Latitude"))
adm2_occ <- get_polygons_2(poly_occ %>% select("Longitude", "Latitude"))

plot_training <- function(plot_ext, legend_pos = 'none'){
  
  mbs_coord <- coord_sf(xlim = plot_ext[1:2],
                        ylim = plot_ext[3:4],
                        expand=FALSE,
                        datum = NA)
  ggplot() +
    geom_sf(data = SEA_simple,
            col='grey85',
            fill='grey85',
            size = 0.3) + 
    geom_raster(data = as.data.frame(mbs_overlaps,xy=TRUE),
                mapping = aes(x =x, y=y, fill = layer)) + 
    
    geom_sf(data = st_as_sf(mbs_over_poly),
            col = rgb(1,1,1,0.2),
            fill = rgb(0,0,0,0),
            size = 0.01) +
    
    mbs_coord +
    
    scale_fill_distiller(palette = "Reds",
                         na.value = NA,
                         direction = 1,
                         name = "Number of presence polygons over pixel",
                         breaks = 1:20,
                         labels = function(i) ifelse(i %% 5 == 0 | i == 1 | i == 18, i, "")) +
    
    geom_point(data = abs_points,
               aes(x=Longitude, y = Latitude, color='a'),
               size = 0.05) +
    
    geom_point(data = occ_points,
               aes(x=Longitude, y = Latitude, color = 'b'),
               size = 0.3) +
    
    scale_color_manual(values = c('a' = "black",'b' = '#48C8D5'),
                       labels = c("Absence", "Presence"),
                       name="Points") +
    
    theme(panel.background = element_rect(fill='white'),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = legend_pos,
          legend.direction = "horizontal") +
    guides(fill = guide_colorsteps(label.position = 'bottom',
                                   title.position = 'top',
                                   ticks = TRUE,
                                   barwidth = 12,
                                   barheight = 0.7,
                                   direction = 'horizontal'))
}

library(cowplot)

p1 <- plot_training(c(99,105,0.75,7.2))
p2 <- plot_training(c(109,120,0.5,7.5))



p_full <- plot_grid(p1, p2,
                    nrow = 2, ncol=1)

plot(p_full)



plot_legend <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(p_full, plot_legend, ncol = 1, rel_heights = c(1, .2))

ggsave("output/figures/MBS_training.pdf",
       width = 6, height=6)



plot_training(extent(mbs_mask), "bottom")

ggsave("output/figures/MBS_training_2.pdf", width = 8, height = 3)
