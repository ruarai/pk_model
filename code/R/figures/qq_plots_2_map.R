

order_plot_data_new <- as.data.frame(
  risk_new * species_extent, xy = TRUE
) %>%
  mutate(value = if_else(layer == 0, NA_real_, layer)) %>%
  drop_na(value) %>%
  mutate(order_val = rank(value))

order_plot_data_old <- as.data.frame(
  fs_risk_mean * species_extent, xy = TRUE
) %>%
  mutate(value = if_else(layer == 0, NA_real_, layer)) %>%
  drop_na(value) %>%
  mutate(order_val = rank(value))






order_plot_data_diff <- order_plot_data_old %>%
  left_join(order_plot_data_new, by = c("x", "y"), suffix = c("_old", "_new")) %>%
  
  mutate(order_val_diff = order_val_new - order_val_old)

plots_common <- list(
  coord_sf(xlim = range(order_plot_data_old$x, na.rm = TRUE),
           ylim = range(order_plot_data_old$y, na.rm = TRUE),
           expand=FALSE,
           datum = NA),
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.7, 0.8), legend.direction = "vertical")
)

ggplot() +
  
  geom_sf(data = SEA_simple,
          col=rgb(0,0,0,0.4),
          fill="grey80",
          size = 0.1) +
  geom_raster(aes(x = x, y = y, fill = order_val_diff),
              order_plot_data_diff) +
  
  colorspace::scale_fill_continuous_diverging(name = "Rank change",
                                              palette = "Red-Green", rev = TRUE,
                                              l1 = 30, l2 = 100, p1 = .9, p2 = 1.1,
                                              
                                              mid = 0,
                                              limits = c(-100000, 100000),
                                              labels = scales::label_comma(),
                                              breaks = c(-100000, 0, 100000)) +
  
  plots_common +
  
  ggtitle("Rank change of mean transmission risk")
