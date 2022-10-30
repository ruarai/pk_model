
library(raster)
library(tidyverse)

library(Matrix)
library(sparseMatrixStats)


run_unique_name <- "final_1"

full_pred_matrix <- readRDS(file = paste0("output/update/", run_unique_name, "_pred_matrix.Rds"))
blank_seasia <- raster('data/clean/raster/SEAsia_extent.grd')


pred_means <- colMeans2(full_pred_matrix)
pred_means[pred_means == 0] <- NA

fs_risk_mean <- raster("data/clean/raster/SEAsia.tif")

fs_means <- getValues(fs_risk_mean)

na_either <- is.na(fs_means) | is.na(pred_means)

quant_new <- tibble(new = pred_means) %>%
  mutate(new_index = row_number()) %>%
  slice(which(!na_either)) %>%
  arrange(new) %>%
  mutate(new_order = row_number() / nrow(.))

quant_old <- tibble(old = fs_means) %>%
  mutate(old_index = row_number()) %>%
  slice(which(!na_either)) %>%
  arrange(old) %>%
  mutate(old_order = row_number() / nrow(.))


quants_plot <- quant_new %>%
  left_join(quant_old, by = c("new_index" = "old_index")) %>%
  sample_n(300000) %>%
  
  mutate(rank_diff = new_order - old_order)

ggplot(quants_plot) +
  annotate(geom = 'segment',
           x = 0, y = 1, xend = 1, yend = 0,
           linetype = 'dotted') +
  annotate(geom = 'segment',
           x = 0, y = 0, xend = 1, yend = 0,
           linetype = 'longdash') +
  annotate(geom = 'segment',
           x = 0, y = 0, xend = 1, yend = -1,
           linetype = 'dotted') +
  
  geom_point(aes(x = old_order, y = rank_diff),
             alpha = 0.2,
             color = '#c7777b',
             stroke = 0,
             size = 0.5,) +
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  
  xlab("Original 2015 model rank") + ylab("Change in rank") +
  
  theme_minimal()


ggplot(quants_plot) +
  geom_histogram(aes(x = new),
                 fill = '#e6a726',
                 alpha = 0.5,
                 binwidth = 0.05) +
  geom_histogram(aes(x = old),
                 fill = '#2880b8',
                 alpha = 0.5,
                 binwidth = 0.05)

ridges_plot <- quants_plot %>%
  pivot_longer(cols = c(new, old))


ggplot(ridges_plot) +
  ggridges::geom_density_ridges(aes(x = value, y = name))

quants_plot




