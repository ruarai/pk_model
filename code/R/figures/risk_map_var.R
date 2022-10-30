# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

risk_brick <- brick("output/update/out_pred_brick_final.grd")
species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

ocean <- risk_brick$mean == 0

risk_map <- function(raster_data) {
  risk_masked <- raster_data * species_extent
  risk_masked[risk_masked == 0] <- NA
  risk_masked[ocean] <- NA
  
  risk_df <- as.data.frame(risk_masked, xy=TRUE)
  colnames(risk_df) <- c("x", "y", "risk")
  
  risk_df <- risk_df %>%
    drop_na(risk)
  
  ggplot(risk_df) +
    
    geom_sf(data = SEA_simple,
            col=NA,
            fill='grey80') +
    
    geom_raster(aes(x=x, y=y, fill = risk)) +
    scale_fill_gradientn(colours=colors_risk,
                         limits=c(0,1),
                         na.value = NA,
                         name = expression(italic("P. knowlesi")~'transmission suitability'),
                         breaks = c(0,1)) +
    
    geom_sf(data = SEA_simple,
            col=rgb(1,1,1,0.3),
            fill=NA,
            size = 0.1) +
    
    coord_sf(xlim = extent(sea_mask)[1:2],
             ylim = extent(sea_mask)[3:4],
             expand=FALSE,
             datum = NA) +
    
    theme(panel.background = element_rect(fill='white'),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = c(0.8,0.7)) +
    guides(fill = guide_colorbar(label.position = 'bottom',
                                 title.position = 'top',
                                 ticks = FALSE,
                                 barwidth = 12,
                                 barheight = 0.7,
                                 direction = 'horizontal'))
}

var_map <- function(raster_data,
                    value_label) {
  risk_var_masked <- raster_data * species_extent
  risk_var_masked[risk_var_masked == 0] <- NA
  risk_var_masked[ocean] <- NA
  
  risk_var_df <- as.data.frame(risk_var_masked, xy=TRUE)
  colnames(risk_var_df) <- c("x", "y", "risk")
  
  risk_var_df <- risk_var_df %>%
    drop_na(risk)
  
  ggplot(risk_var_df) +
    
    geom_sf(data = SEA_simple,
            col=NA,
            fill='grey80') +
    
    geom_raster(aes(x=x, y=y, fill = risk)) +
    
    scale_fill_distiller(value_label,
                         type = 'seq',
                         palette = "Reds",
                         direction = 1,
                         na.value = NA) +
    
    geom_sf(data = SEA_simple,
            col=rgb(0,0,0,0.5),
            fill=NA,
            size = 0.1) +
    
    coord_sf(xlim = extent(sea_mask)[1:2],
             ylim = extent(sea_mask)[3:4],
             expand=FALSE,
             datum = NA) +
    
    theme(panel.background = element_rect(fill='white'),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = c(0.8,0.7)) +
    guides(fill = guide_colorbar(label.position = 'bottom',
                                 title.position = 'top',
                                 ticks = FALSE,
                                 barwidth = 12,
                                 barheight = 0.7,
                                 direction = 'horizontal'))
}


p_mean <- risk_map(risk_brick$mean) +
  ggtitle("Mean")
p_var <- var_map(risk_brick$var,
                 "Variance") +
  ggtitle("Variance")

p_q5 <- risk_map(risk_brick$q5) +
  ggtitle("5th percentile")
p_q95 <- risk_map(risk_brick$q95)+
  ggtitle("95th percentile")

p_q25 <- risk_map(risk_brick$q25) +
  ggtitle("25th percentile")
p_q75 <- risk_map(risk_brick$q75) +
  ggtitle("75th percentile")

p_median <- risk_map(risk_brick$q50) +
  ggtitle("Median")
p_IQR <- var_map(risk_brick$q75 - risk_brick$q25,
                 "Interquartile range") +
  ggtitle("Interquartile range")

p_full <- cowplot::plot_grid(p_mean, p_var,
                             p_median, p_IQR,
                             p_q25, p_q75,
                             p_q5, p_q95,
                             
                             ncol = 2)

ggsave("output/figures/SEA_risk_supp.png",
       p_full,
       width = 8.3 * 1.5, height = 11.7 * 1.5)
