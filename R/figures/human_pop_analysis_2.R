

library(raster)
library(tidyverse)

library(seegSDM)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


pred_SEA <- brick("data/clean/runs/4a/raster_updated/prediction_SEA")

pred_MBS <- brick("data/clean/runs/4a/raster_updated/prediction_MBS")

human_pop <- raster("data/clean/raster_updated/human_pop_MBS.grd")

mbs_mask <- raster("data/clean/raster/mbs_mask")
mbs_mask[mbs_mask == 1] <- 0

mbs_mask <- raster::extend(mbs_mask, pred_SEA)
mbs_mask[is.na(mbs_mask)] <- 1

if(extent(pred_SEA) != extent(mbs_mask))
  pred_SEA <- pred_SEA * mbs_mask
names(pred_SEA) <- names(pred_MBS)


vals_SEA <- getValues(pred_SEA)
vals_MBS <- getValues(pred_MBS)


pick_cols <- c("human_pop",
               "SEA_access_healthcare",
               "treecover",
               "nemestrina",
               "SRTM_elevation",
               "Pf_temp")



df_SEA <- vals_SEA %>% 
  data.frame() %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "SEA",
         type = "predictor")


df_MBS <- vals_MBS %>% 
  data.frame() %>%
  pivot_longer(cols = all_of(pick_cols))  %>%
  mutate(source = "MBS",
         type = "predictor")

df_absence <- read.csv("data/clean/occurrence/ALL_background_points.csv") %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(all_of(pick_cols)) %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "background",
         type = "absence")



## Background points:

occ_files <- c("MBS_FS_B_2005-2014.csv",
               "MBS_MT_point_2007-2018.csv",
               "MBS_MT_polygon_2007-2018.csv")

occ_files <- str_c("data/clean/occurrence/pk_present/", occ_files)

occ_data <- bind_rows(lapply(occ_files, read.csv))


df_points <- occ_data %>%
  filter(Geometry_type == "point") %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(all_of(pick_cols)) %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "points",
         type = "presence")




poly_points <- occ_data %>%
  filter(Geometry_type == "polygon") %>%
  group_by(Unique_ID) %>%
  sample_n(50, replace=TRUE) %>%
  ungroup() %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(all_of(pick_cols)) %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "polygons",
         type = "presence")
  



df_all <- bind_rows(df_absence,
                    df_points,
                    poly_points,
                    df_SEA %>% sample_n(10000),
                    df_MBS %>% sample_n(10000))



ggplot(df_all %>%
         filter(name == "human_pop",
                source != "SEA")) +
  geom_histogram(aes(x = value, fill = type), bins = 100) +
  facet_grid(rows = vars(source),
             cols = vars(name),
             scales="free") +
  scale_x_log10()
  

ggplot(df_all %>%
         filter(name == "SEA_access_healthcare",
                source != "SEA")) +
  geom_histogram(aes(x = value, fill = type), bins = 100) +
  facet_grid(rows = vars(source),
             cols = vars(name),
             scales="free") +
  scale_x_log10()


