

library(raster)
library(tidyverse)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


pred_SEA <- brick("data/clean/runs/4c/raster_updated/prediction_SEA")

pred_MBS <- brick("data/clean/runs/4c/raster_updated/prediction_MBS")


mbs_mask <- raster("data/clean/raster/mbs_mask")
mbs_mask[mbs_mask == 1] <- 0

mbs_mask <- raster::extend(mbs_mask, pred_SEA)
mbs_mask[is.na(mbs_mask)] <- 1

pred_SEA <- pred_SEA * mbs_mask
names(pred_SEA) <- names(pred_MBS)


vals_SEA <- getValues(pred_SEA)
vals_MBS <- getValues(pred_MBS)





df_SEA <- vals_SEA %>% 
  data.frame() %>%
  mutate(pixel = row_number()) %>%
  pivot_longer(cols = -matches("pixel")) %>%
  mutate(source = "SEA")


df_MBS <- vals_MBS %>% 
  data.frame() %>%
  mutate(pixel = row_number()) %>%
  pivot_longer(cols = -matches("pixel")) %>%
  mutate(source = "MBS")


df_all <- bind_rows(df_SEA, df_MBS)

df_sampled <- df_all %>%
  group_by(name, source) %>%
  sample_n(10000)



ggplot(df_sampled) +
  geom_histogram(aes(x = value, fill = source, y = stat(count) / sum(count))) +
  facet_grid(rows = vars(source),
             cols = vars(name),
             scales="free",
             labeller = label_wrap_gen(width=12)) +
  theme_bw() +
  coord_cartesian(ylim=c(0,0.003))
  


library(scales)

df_joined <- left_join(x = filter(df_all, name == "human_pop"),
                       y = filter(df_all, name != "human_pop"),
                       by = c("pixel", "source")) %>%
  group_by(source, name.y) %>%
  sample_n(10000) %>%
  mutate(value.x = rescale(value.x),
         value.y = rescale(value.y))


ggplot(df_joined) +
  geom_point(aes(x = value.x, y = value.y, col = source), size=0.2) +
  facet_grid(rows = vars(source),
             cols = vars(name.y),
             scales="free_y") +
  geom_smooth(aes(x = value.x, y = value.y), method="lm")




