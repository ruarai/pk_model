

mess_binary_old <- raster("data/clean/raster/mess_binary_FS.tif")



plot(mess_binary_old)


mess_diff_data <- tibble(
  as.data.frame(mess_binary, xy = TRUE) %>% rename(mess_new = layer),
  as.data.frame(mess_binary_old) %>% rename(mess_old = mess_binary_FS)
) %>%
  mutate(mess_new = mess_new == 1,
         mess_old = mess_old == 1) %>%
  
  mutate(mess_both = mess_new & mess_old) %>%
  filter(mess_new | mess_old) %>%
  
  mutate(mess_label = case_when(
    mess_both ~ "both",
    mess_new ~ "new",
    mess_old ~ "old",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("old", "both", "new")))


ggplot() +
  
  geom_sf(data = SEA_simple,
          col=rgb(1,1,1,0.3),
          fill="grey80",
          size = 0.1) +
  geom_raster(aes(x = x, y = y, fill = mess_label),
              mess_diff_data) +
  
  scale_fill_brewer(
    "MESS",
    palette = "YlGnBu",
    labels = c(
      "new" = "This work only",
      "both" = "This work and 2015 model",
      "old" = "2015 model only"
    )
  ) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  theme(panel.background = element_rect(fill='white', colour = '#e4e4e4', size = 1.3),
        legend.key = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.7, 0.7),
        legend.direction = "vertical",
        legend.background = element_rect(fill = 'grey98'))
