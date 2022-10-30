

library(raster)
library(tidyverse)



source("code/R/figures/maps_common.R")

species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

risk_brick <- brick("output/update/out_pred_brick_final.grd")

risk_masked <- risk_brick$mean * species_extent
risk_masked[risk_masked == 0] <- NA

risk_raster_old <- raster("data/clean/raster/SEAsia.tif")
risk_raster_old_masked <- risk_raster_old * species_extent
risk_raster_old_masked[risk_raster_old_masked == 0] <- NA


risk_new <- as.data.frame(
  risk_masked * mbs_mask, xy = TRUE
) %>%
  rename(value = layer)
risk_old <- as.data.frame(
  risk_raster_old_masked * mbs_mask, xy = TRUE
) %>%
  rename(value = layer)

slope_new <- as.data.frame(
  terrain(risk_masked * mbs_mask, neighbours = 4),
  xy = TRUE
) %>%
  rename(value = slope)


slope_old <- as.data.frame(
  terrain(risk_raster_old_masked * mbs_mask, neighbours = 4),
  xy = TRUE
) %>%
  rename(value = slope)

slope_max <- max(c(slope_new$value, slope_old$value), na.rm = TRUE)

scale_fill_slope <- list(
  scale_fill_viridis_c(na.value = "white",
                       option = "D",
                       limits = c(0, slope_max))
)

scale_fill_risk <- list(
  scale_fill_gradientn(colours=colors_risk,
                       limits=c(0,1),
                       na.value = NA,
                       name = expression(italic("P. knowlesi")~'transmission suitability'),
                       breaks = c(0,1))
)

plots_common <- list(
  coord_sf(xlim = c(extent(mbs_mask)[1], 105),
           ylim = extent(mbs_mask)[3:4],
           expand=FALSE,
           datum = NA),
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")
)

cowplot::plot_grid(

  ggplot(risk_old) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    plots_common +
    
    scale_fill_risk +

    ggtitle("Predicted risk", "Old model")
  ,

  ggplot(risk_new) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    plots_common +
    
    scale_fill_risk +

    ggtitle("", "New model")
  ,

  ggplot(slope_old) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    plots_common +
    
    scale_fill_slope +
    
    ggtitle("Risk slope", "Old model")
  ,
  
  ggplot(slope_new) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    plots_common +
    
    scale_fill_slope +
    
    ggtitle("", "New model")
  ,
  
  ncol = 2
)




slope_binary_new <- slope_new %>%
  filter(value > quantile(value, 0.75, na.rm = TRUE))

slope_binary_old <- slope_old %>%
  filter(value > quantile(value, 0.75, na.rm = TRUE))

slope_binary_both <- slope_binary_new %>%
  inner_join(slope_binary_old, by = c("x", "y"))

ggplot() +
  geom_raster(aes(x = x, y = y),
              slope_binary_new,
              
              fill = ggokabeito::palette_okabe_ito(2),
              alpha = 0.8) +
  geom_raster(aes(x = x, y = y),
              slope_binary_old,
              
              fill = ggokabeito::palette_okabe_ito(1),
              alpha = 0.8) +
  geom_raster(aes(x = x, y = y),
              slope_binary_both,
              
              fill = ggokabeito::palette_okabe_ito(9),
              alpha = 0.8) +
  plots_common

