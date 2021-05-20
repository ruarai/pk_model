
library(raster)
library(tidyverse)

library(viridis)
library(ggplot2)
source("code/R/figures/maps_common.R")
plot_raster <- function(recent_loss, title, ext){
  df <- as.data.frame(recent_loss, xy=TRUE)
  names(df) <- c("x","y","layer")
  ggplot() +
    geom_raster(data = df,
                mapping = aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Proportion forest coverage loss over time period",
                       limits = c(0, 1)) +
    xlab("Longitude") + ylab("Latitude") +
    
    geom_sf(data = MBS_simple,
            col=rgb(1,1,1,0.3),
            fill=NA,
            size = 0.1) +
    
    coord_sf(xlim = ext[1:2],
             ylim = ext[3:4],
             expand=FALSE,
             datum = NA) +
    
    theme(legend.position = "none",
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(size = 10)) +
    ggtitle(title)
}

forestloss_stack <- brick("data/raw/covariate_production/temporal_final/lossyear_stack")




p1 <- plot_raster(forestloss_stack[[19]], "Last 1 year (MBS)", extent(mbs_mask))
p2 <- plot_raster(sum(forestloss_stack[[(20-3):19]]), "Last 3 years (MBS)", extent(mbs_mask))
p3 <- plot_raster(sum(forestloss_stack[[(20-5):19]]), "Last 5 years (MBS)", extent(mbs_mask))


p4 <- plot_raster(forestloss_stack[[19]], "Last 1 year (Sabah)",c(115.34,119.27,4.13,7.36))
p5 <- plot_raster(sum(forestloss_stack[[(20-3):19]]), "Last 3 years (Sabah)", c(115.34,119.27,4.13,7.36))
p6 <- plot_raster(sum(forestloss_stack[[(20-5):19]]), "Last 5 years (Sabah)", c(115.34,119.27,4.13,7.36))

library(cowplot)
p_legend <- get_legend(
  p1 + 
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          title = element_text(size = 11,
                               vjust = 1)) +
    guides(fill = guide_colorbar(barheight = 0.7))
)

p_top <- plot_grid(p1, p2, p3,
                   p4, p5, p6,
                   byrow = F,
                   nrow = 3,
                   rel_widths = c(2.2,1))


plot_grid(p_top,
          p_legend,
          nrow=2,
          ncol=1,
          rel_heights = c(3, .2))

ggsave("output/figures/MBS_forestloss.pdf", height = 5, width = 6)
