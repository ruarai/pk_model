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




cowplot::plot_grid(
  
  ggplot(risk_df) +
    
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
    
    plots_common +
    
    ggtitle(parse(text = "bold(A)")),
  
  ggplot(risk_var_df) +
    
    geom_sf(data = SEA_simple,
            col=NA,
            fill='grey80') +
    
    geom_raster(aes(x=x, y=y, fill = sqrt(var))) +
    
    annotate("rect", xmin = 84, ymin = -12, ymax = -6.9, xmax = 105,
             fill = "grey98") +
    
    scale_fill_gradientn(
      name = "Prediction standard deviation",
      colours = cartography::carto.pal("harmo.pal", 20),
      breaks = c(0.01, 0.15, sqrt(max_var)),
      labels = c("0", "0.15", round(sqrt(max_var), 2)),
      na.value = NA
    ) +
    
    plots_common +
    
    ggtitle(parse(text = "bold(B)"))
)


ggsave("output/figures/SEA_risk.pdf",
       width = 13, height = 6)

