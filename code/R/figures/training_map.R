
# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

test_points <- read.csv("data/clean/occurrence/testing/test_points.csv")
stopifnot(all(test_points$PA == 1))


test_poly_data <- read.csv("data/clean/occurrence/testing/test_poly.csv")
test_poly_data$PA <- factor(test_poly_data$PA)

test_poly_shape <- readRDS("data/clean/occurrence/testing/poly_shapes.Rds")
rownames(test_poly_data) <- names(test_poly_shape)

test_poly_sdf <- SpatialPolygonsDataFrame(test_poly_shape, test_poly_data)

mbs_extent <- c(extent(mbs_mask)[1:2] + c(0, 0.1),
                extent(mbs_mask)[3:4] + c(-0.1, 0))



p_test <- ggplot() +
  geom_sf(data = SEA_simple,
          col='grey80',
          size = 0.2) +
  
  geom_sf(data = MBS_simple,
          aes(fill = 'training'),
          col = rgb(0,0,0,0)) +
  
  geom_sf(data = sf::st_as_sf(test_poly_sdf),
          mapping = aes(fill = PA),
          color=rgb(1,1,1,0.3),
          size = 0.2) +
  
  geom_blank(data = tibble(Nodata = "2"),
             mapping = aes(fill = Nodata)) +
  
  scale_fill_manual(values = c("0" = "#a3be7a", "1" = "#d79da0", "2" = "grey90", 'training' = 'grey40'),
                    labels = c("0" = "Absence polygons",
                               "1" = "Presence polygons",
                               "2" = "No data", 'training' = 'Training region'),
                    name = NULL) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  geom_point(data = test_points,
             aes(x = Longitude, y = Latitude, color = 'A'),
             pch = 4,
             size = 0.9) +
  
  annotate("rect", xmin = mbs_extent[1], xmax = mbs_extent[2], ymin = mbs_extent[3], ymax = mbs_extent[4],
           fill = 'transparent', colour = 'grey80') +
  
  scale_color_manual(values = c("A" = "#951d66"),
                     labels = "Presence points",
                     name = NULL) +
  
  theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.8,0.7),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.direction = "vertical",
        legend.background = element_rect(fill = 'grey98'))


p_test

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

mbs_coord <- coord_sf(xlim = extent(mbs_mask)[1:2] + c(0, 0.1),
                      ylim = extent(mbs_mask)[3:4] + c(-0.1, 0),
                      expand=FALSE,
                      datum = NA)
p_training <- ggplot() +
  geom_sf(data = SEA_simple,
          col='grey85',
          fill='#e5e5e5',
          size = 0.3) + 
  geom_tile(data = as.data.frame(mbs_overlaps,xy=TRUE),
              mapping = aes(x =x, y=y, fill = layer)) +
  
  mbs_coord +
  
  scale_fill_stepsn(colours = c("#f4e2e3", "#6e0142"),
                    breaks = 0:20,
                    limits = c(0, 20),
                    name = "Presence polygon density",
                    labels = function(i) ifelse(i == 1 | i == 10 | i == 19, i, ""),
                    na.value = NA) +
  
  geom_point(data = occ_points,
             aes(x=Longitude, y = Latitude, color = 'b'),
             size = 0.8) +
  
  geom_point(data = occ_points,
             aes(x=Longitude, y = Latitude, color = 'b'),
             color = 'white',
             size = 0.2, stroke = 0.3) +
  
  scale_color_manual(values = c('b' = 'black'),
                     labels = c('b' = "Presence points"),
                     name="") +
  
  theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
        legend.key = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = c(0.5, 0.7),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = 'grey98')) +
  guides(fill = guide_colorsteps(label.position = 'bottom',
                                 title.position = 'top',
                                 label.hjust = 0.5,
                                 barwidth = 12,
                                 barheight = 0.7,
                                 direction = 'horizontal'),
         colour = guide_legend(override.aes = list(shape = 1, size = 1, colour = 'black')))

p_training


mess_df <- read_rds("output/update/mess_df.rds")

p_mess <- ggplot(mess_df) +
  geom_raster(aes(x=x, y=y, fill = layer)) +
  scale_fill_manual(values = c('0' = '#e5e5e5', '1' = '#5b798b'),
                    name = NULL,
                    labels = c('0' = "Extrapolation", '1' = "Interpolation"),
                    na.translate = FALSE) +
  
  geom_sf(data = SEA_simple,
          col=rgb(1,1,1,0.3),
          fill=NA,
          size = 0.1) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  annotate("rect", xmin = mbs_extent[1], xmax = mbs_extent[2], ymin = mbs_extent[3], ymax = mbs_extent[4],
           fill = 'transparent', colour = 'grey80') +
  
  theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
        legend.key = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.7, 0.7),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.direction = "vertical",
        legend.background = element_rect(fill = 'grey98'))



cowplot::plot_grid(
  
  p_training + ggtitle(parse(text = "bold(A)~-~Training~region")),
  
  cowplot::plot_grid(
    p_test + ggtitle(parse(text = "bold(B)~-~Evaluation~region")),
    p_mess + ggtitle(parse(text = "bold(C)~-~MESS")),
    
    ncol = 2,
    rel_heights = c(1, 1.8)
  ),
  
  ncol = 1
  
)
ggsave("output/figures/training_testing.pdf",
       width = 9 * 0.65 * 2, height = 9 * 0.92,
       bg = "white")

