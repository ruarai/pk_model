
library(raster)
library(tidyverse)

library(viridis)
library(ggplot2)
source("code/R/figures/maps_common.R")
plot_raster <- function(recent_loss, title, ext, hl = NULL){
  df <- as.data.frame(recent_loss, xy=TRUE)
  names(df) <- c("x","y","layer")
  
  p <- ggplot() +
    geom_raster(data = df,
                mapping = aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis(na.value = rgb(1,1,1,alpha=0),
                       name = "Proportion forest coverage loss over time period",
                       limits = c(0, 1)) +
    xlab("Longitude") + ylab("Latitude") +
    
    coord_sf(xlim = ext[1:2],
             ylim = ext[3:4],
             expand=FALSE,
             datum = NA) +
    
    theme(legend.position = "none",
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(size = 9)) +
    ggtitle(title)
  
  
  if(!is.null(hl)){
    p <- p +
      geom_rect(aes(xmin=hl[1], xmax=hl[2],
                    ymin=hl[3], ymax=hl[4]),
                fill=NA,
                colour='blue')
  }
  
  p
}

forestloss_stack <- brick("data/raw/covariate_production/temporal_final/lossyear_stack")

idn <- SEA_simple[SEA_simple$COUNTRY_ID == "IDN",]

ext_wkal <- c(114.484,-1.707, 120.199,1.9)[c(1,3,2,4)]

p1 <- plot_raster(forestloss_stack[[19]],
                  "Last 1 year (Indonesia)", 
                  extent(idn),
                  ext_wkal)
p2 <- plot_raster(sum(forestloss_stack[[(20-3):19]]),
                  "Last 3 years (Indonesia)",
                  extent(idn),
                  ext_wkal)
p3 <- plot_raster(sum(forestloss_stack[[(20-5):19]]),
                  "Last 5 years (Indonesia)",
                  extent(idn),
                  ext_wkal)


p4 <- plot_raster(forestloss_stack[[19]],
                  "Last 1 year (East Kalimantan)",
                  ext_wkal)
p5 <- plot_raster(sum(forestloss_stack[[(20-3):19]]),
                  "Last 3 years (East Kalimantan)",
                  ext_wkal)
p6 <- plot_raster(sum(forestloss_stack[[(20-5):19]]), 
                  "Last 5 years (East Kalimantan)",
                  ext_wkal)

library(cowplot)
library(gridExtra)
p_legend <- get_legend(
  p1 + 
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          title = element_text(size = 9,
                               vjust = 1)) +
    guides(fill = guide_colorbar(barheight = 0.7))
)

p_top <- plot_grid(p1, p2, p3,
                   p4, p5, p6,
                   byrow = F,
                   nrow = 3,
                   rel_widths = c(2.2,1),
                   align = 'v',
                   axis = 'bt')


p_full <- plot_grid(p_top,
          p_legend,
          nrow=2,
          ncol=1,
          rel_heights = c(3, .2))



p_with_margin <- grid.arrange(p_full,
                              vp=grid::viewport(width=0.9, height=0.8),
                              top = "Comparison of forest loss time periods")

ggsave(plot = p_with_margin,
       "output/figures/MBS_forestloss.pdf",
       height = 8.27, width = 11.69)
