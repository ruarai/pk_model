# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

risk_brick <- brick("output/update/out_pred_brick_final.grd")

risk_masked <- risk_brick$mean * species_extent
risk_masked[risk_masked == 0] <- NA

risk_var_masked <- risk_brick$var * species_extent
risk_var_masked[risk_var_masked == 0] <- NA

risk_df <- as.data.frame(risk_masked, xy=TRUE)
colnames(risk_df) <- c("x", "y", "risk")

risk_var_df <- as.data.frame(risk_var_masked, xy=TRUE)
colnames(risk_var_df) <- c("x", "y", "var")



max_var <- max(risk_var_df$var, na.rm = TRUE)


interior_borders <- sf::read_sf("data/raw/admin_maps/interior/LSIB.shp")
interior_borders_simple <- sf::st_simplify(interior_borders, dTolerance = 0.01)


test_points <- read.csv("data/clean/occurrence/testing/test_points.csv")
stopifnot(all(test_points$PA == 1))


test_poly_data <- read.csv("data/clean/occurrence/testing/test_poly.csv")
test_poly_data$PA <- factor(test_poly_data$PA)

test_poly_shape <- readRDS("data/clean/occurrence/testing/poly_shapes.Rds")
rownames(test_poly_data) <- names(test_poly_shape)

test_poly_sdf <- SpatialPolygonsDataFrame(test_poly_shape, test_poly_data)

mbs_extent <- c(extent(mbs_mask)[1:2] + c(0, 0.1),
                extent(mbs_mask)[3:4] + c(-0.1, 0))

plots_common <- list(
  geom_sf(data = interior_borders_simple,
          pch = '.',
          col=rgb(0,0,0,0.8),
          fill=NA,
          size = 0.4),
  
  coord_sf(xlim = c(84, 130),
           ylim = c(-12, 30),
           expand=FALSE,
           datum = NA),
  
  theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = 'grey98')),
  
  guides(fill = guide_colorbar(label.position = 'bottom',
                               title.position = 'top',
                               ticks = FALSE,
                               barwidth = 12,
                               barheight = 0.7,
                               direction = 'horizontal'))
)


presence <- sf::st_as_sf(test_poly_sdf[test_poly_sdf$PA == 1, ])
absence <-  sf::st_as_sf(test_poly_sdf[test_poly_sdf$PA == 0, ])

presence_raster <- raster::rasterize(
  presence,
  sea_mask
) %>%
  as.data.frame(xy = TRUE) %>%
  drop_na(layer_PA)
absence_raster <- raster::rasterize(
  absence,
  sea_mask
) %>%
  as.data.frame(xy = TRUE) %>%
  drop_na(layer_PA)



p_presence <- ggplot(risk_df) +
  
  geom_sf(data = SEA_simple,
          col=NA,
          fill="grey80") +
  
  geom_raster(aes(x=x, y=y, fill = risk)) +
  scale_fill_gradientn(colours=colors_risk,
                       limits=c(0, max(risk_df$risk, na.rm = TRUE)),
                       na.value = NA,
                       name = expression(italic("P. knowlesi")~'transmission suitability'),
                       breaks = c(0,max(risk_df$risk, na.rm = TRUE)),
                       labels = c(0, 1)) +
  
  geom_raster(aes(x, y),
              fill = "blue", alpha = 0.3,
              data = presence_raster) +
  
  geom_sf(data = MBS_simple,
          fill = "grey40",
          col = rgb(0,0,0,0))  +
  
  geom_point(data = test_points,
             aes(x = Longitude, y = Latitude, color = 'A'),
             colour = "blue",
             pch = 4,
             alpha = 0.3,
             size = 0.9) +
  
  plots_common +
  
  ggtitle(parse(text = "bold(A)~-~Evaluation~region~occurrence~data"))

p_presence

p_absence <- ggplot(risk_df) +
  
  geom_sf(data = SEA_simple,
          col=NA,
          fill="grey80") +
  
  geom_raster(aes(x=x, y=y, fill = risk)) +
  scale_fill_gradientn(colours=colors_risk,
                       limits=c(0, max(risk_df$risk, na.rm = TRUE)),
                       na.value = NA,
                       name = expression(italic("P. knowlesi")~'transmission suitability'),
                       breaks = c(0,max(risk_df$risk, na.rm = TRUE)),
                       labels = c(0, 1)) +
  
  geom_raster(aes(x, y),
              fill = "blue", alpha = 0.3,
              data = absence_raster)  +
  
  geom_sf(data = MBS_simple,
          fill = "grey40",
          col = rgb(0,0,0,0)) +
  
  plots_common +
  
  ggtitle(parse(text = "bold(A)~-~Evaluation~region~absence~data"))


cowplot::plot_grid(
  p_presence, p_absence, ncol = 2
)

ggsave("output/figures/SEA_risk_pres_abs.pdf",
       width = 13, height = 6)

