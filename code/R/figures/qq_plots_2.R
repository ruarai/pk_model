# Map libraries
require(raster)
require(tidyverse)

source("code/R/figures/maps_common.R")

species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")

risk_brick <- brick("output/update/out_pred_brick_final.grd")

risk_masked <- risk_brick$mean * species_extent
risk_masked[risk_masked == 0] <- NA


source("code/R/figures/admin_common.R")

plot(raster::crop(risk_brick$mean, mbs_mask))


fs_risk_mean <- raster("data/clean/raster/SEAsia.tif")





risk_new <- risk_brick$mean
risk_new[risk_new == 0] <- NA

mean_vals_new <- getValues(risk_new * species_extent)
mean_vals_old <- getValues(fs_risk_mean * species_extent)

mean_vals_new[mean_vals_new == 0] <- NA
mean_vals_old[mean_vals_old == 0] <- NA

hist(mean_vals_new)
hist(mean_vals_old)


p_data <- tibble(
  new = quantile(mean_vals_new, probs = seq(0, 1, by = 0.0025), na.rm = TRUE),
  old = quantile(mean_vals_old, probs = seq(0, 1, by = 0.0025), na.rm = TRUE)
)


p_data_summ <- tibble(
  new = mean_vals_new,
  old = mean_vals_old
) %>%
    pivot_longer(c("new", "old")) %>%
    group_by(name) %>%
    summarise(q25 = quantile(value, 0.25, na.rm = TRUE),
              q50 = quantile(value, 0.50, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE)) %>%
  pivot_longer(-name, names_to = "quant")

cowplot::plot_grid(
  
  ggplot(tibble(x = mean_vals_new)) +
    geom_histogram(aes(x = x, y = ..count../sum(..count..)),
                   binwidth = 0.01, fill = '#5F788B') +
    
    geom_rug(aes(x = value), 
             p_data_summ %>% filter(name == "new"),
             size = 0.7,
             length = unit(5, "cm"),
             linetype = "dotted",
             colour = "white") +
    
    coord_cartesian(xlim = c(0, 0.97), ylim = c(0, 0.04)) +
    
    xlab(NULL) + ylab(NULL) +
    
    ggtitle(parse(text = "bold(A)")) +
    
    scale_y_continuous(breaks = scales::breaks_extended(3)) +
    
    xlab("Transmission suitability\n(2020 model, current work)") +
    
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()),
  
  ggplot(tibble(x = mean_vals_old)) +
    geom_histogram(aes(x = x, y = ..count../sum(..count..)),
                   binwidth = 0.01, fill = '#C3767A') +
    
    geom_rug(aes(x = value), 
             p_data_summ %>% filter(name == "old"),
             size = 0.7,
             length = unit(5, "cm"),
             linetype = "dotted",
             colour = "white") +
    
    coord_cartesian(xlim = c(0, 0.9), ylim = c(0, 0.04)) +
    
    xlab(NULL) + ylab(NULL) +
    
    ggtitle(NULL) +
    
    scale_y_continuous(breaks = scales::breaks_extended(3)) +
    
    xlab("Transmission suitability\n(2015 model)") +
    
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()),
  
  
  ggplot(p_data) +
    
    geom_abline(slope = 1, intercept = 0,
                linetype = 'dashed') +
    geom_line(aes(x = old, y = new), color = 'black', size = 0.8) +
    geom_point(aes(x = old, y = new), color = 'black', size = 1,
               p_data %>% slice(c(1, 401))) +
    
    xlab("Transmission suitability\n(2015 model)") + ylab("Transmission suitability\n(2020 model, current work)") +
    
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    
    ggtitle(parse(text = "bold(B)")) +
    
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank()),
  
  nrow = 1,
  
  align = "h", axis = "tb"
)

ggsave(
  "output/figures/qq_plot.png",
  width = 10,
  height = 3.7,
  bg = "white"
)



