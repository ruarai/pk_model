

library(tidyverse)

predictions <- read_rds("output/update/final_1_pred_matrix.Rds")


species_extent <- raster("data/clean/raster_updated/reservoir_vector_extent")


valid_ix <- which(getValues(species_extent == 1))

sample_ix <- sample(valid_ix, size = 100)




plot_mean <- predictions[1:500, sample_ix] %>%
  as.matrix() %>%
  `colnames<-`(str_c("point_", seq(1, length(sample_ix)))) %>%
  as_tibble() %>%
  mutate(bootstrap = 1:n(),
         .before = 1) %>%
  
  mutate(across(
    starts_with("point_"),
    
    ~ cumsum(.) / 1:n()
  )) %>%

  mutate(across(
    starts_with("point_"),

    ~ . - .[n()]
  )) %>%
  
  pivot_longer(
    cols = -c(bootstrap)
  )


p_mean <- ggplot(plot_mean) +
  geom_line(aes(x = bootstrap, y = value, group = name),
            color = ggokabeito::palette_okabe_ito(2),
            alpha = 0.3,
            size = 0.4) +
  
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(-0.25, 0.25)) +
  
  xlab("Number of bootstraps") +
  ylab("Difference") +
  
  ggtitle(NULL, "Predicted mean with increasing number of bootstraps (as difference from final value)")


cumulative_var <- function(x) {
  y <- rep(0, times = length(x))
  
  for(i in 2:length(x)) {
    y[i] <- quantile(x[1:i], 0.95)
  }
  y
}

plot_q95 <- predictions[1:500, sample_ix] %>%
  as.matrix() %>%
  `colnames<-`(str_c("point_", seq(1, length(sample_ix)))) %>%
  as_tibble() %>%
  mutate(bootstrap = 1:n(),
         .before = 1) %>%
  
  mutate(across(
    starts_with("point_"),
    
    ~ cumulative_var(.)
  )) %>%
  
  mutate(across(
    starts_with("point_"),
    
    ~ . - .[n()]
  )) %>%
  
  pivot_longer(
    cols = -c(bootstrap)
  )


p_q95 <- ggplot(plot_q95) +
  geom_line(aes(x = bootstrap, y = value, group = name),
            color = ggokabeito::palette_okabe_ito(3),
            alpha = 0.3,
            size = 0.4) +
  
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(-0.25, 0.25)) +
  
  xlab("Number of bootstraps") +
  ylab("Difference") +
  
  ggtitle(NULL, "Predicted 95th percentile with increasing number of bootstraps (as difference from final value)")


cowplot::plot_grid(
  p_mean,
  p_q95,
  ncol = 1
)



ggsave(
  "output/figures/bootstrap_effect.png",
  width = 9,
  height = 6,
  bg = "white"
)
