

# Map libraries
library(raster)
library(tidyverse)

source("code_ruarai/R/figures/maps_common.R")

data_all <- read.csv("data/clean/occurrence/data_all.csv")

host_col <- which(colnames(data_all) == "Host_species")

data_covs <- data_all %>%
  select((host_col+1):last_col())


pred_raster <- brick("data/clean/raster_updated/prediction_SEA")



mess_raster <- dismo::mess(pred_raster, data_covs, full = TRUE)


mess_binary <- (mess_raster[['rmess']] >= 0) * (sea_mask)
mess_df <- as.data.frame(mess_binary, xy=TRUE)
mess_df$layer <- factor(mess_df$layer)

ggplot(mess_df) +
  geom_raster(aes(x=x, y=y, fill = layer)) +
  scale_fill_manual(values = c('0' = '#98bfd9', '1' = '#cc331a'),
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
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.7,0.7))


ggsave("output/figures/SEA_mess.pdf", width = 6, height = 4)
