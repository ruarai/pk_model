

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
         type = "predictor") %>%
  select(c(value,type,source))


df_MBS <- vals_MBS %>% 
  data.frame() %>%
  pivot_longer(cols = all_of(pick_cols))  %>%
  mutate(source = "MBS",
         type = "predictor") %>%
  select(c(value,type,source))

df_absence <- read.csv("data/clean/occurrence/ALL_background_points.csv") %>%
  filter(Host == "human" | Host == "monkey") %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(all_of(pick_cols)) %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "background",
         type = "absence")



## Background points:

occ_data <- read.csv("data/clean/occurrence/pk_present/ALL_occ_thinned.csv")


df_points <- occ_data %>%
  filter(Geometry_type == "point") %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(c(all_of(pick_cols), Unique_ID)) %>%
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
  select(c(all_of(pick_cols), Unique_ID)) %>%
  pivot_longer(cols = all_of(pick_cols)) %>%
  mutate(source = "polygons",
         type = "presence")
  



df_all <- bind_rows(df_absence,
                    df_points,
                    poly_points,
                    df_MBS)



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

ggplot(df_all %>%
         filter(!is.na(name),source != "SEA")) +
  geom_histogram(aes(x = value, fill = type), bins = 100) +
  facet_grid(rows = vars(source),
             cols = vars(name),
             scales="free") +
  scale_x_log10()


df_data <- bind_rows(poly_points, df_points)


df_mbs_vals <- vals_MBS %>%
  data.frame() %>%
  select(human_pop, SEA_access_healthcare) %>%
  drop_na() %>%
  mutate(log_human_pop = log(human_pop),
         cell_n = row_number()) %>%
  pivot_longer(cols = c(human_pop, log_human_pop, SEA_access_healthcare)) %>%
  full_join(x = .,
            y = .,
            by = c("cell_n")) %>%
  mutate(source = "MBS")



df_mbs_points <- occ_data %>%
  filter(Geometry_type == "point") %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(human_pop, SEA_access_healthcare) %>%
  drop_na() %>%
  mutate(log_human_pop = log(human_pop),
         cell_n = row_number())%>%
  pivot_longer(cols = c(human_pop, log_human_pop, SEA_access_healthcare)) %>%
  full_join(x = .,
            y = .,
            by = c("cell_n")) %>%
  mutate(source = "points")



df_mbs_poly <- occ_data %>%
  filter(Geometry_type == "polygon") %>%
  group_by(Unique_ID) %>%
  sample_n(50, replace=TRUE) %>%
  ungroup() %>%
  bind_cols(.,
            raster::extract(pred_MBS, (.) %>% select("Longitude", "Latitude")) %>%
              data.frame()) %>%
  select(human_pop, SEA_access_healthcare) %>%
  drop_na() %>%
  mutate(log_human_pop = log(human_pop),
         cell_n = row_number())%>%
  pivot_longer(cols = c(human_pop, log_human_pop, SEA_access_healthcare)) %>%
  full_join(x = .,
            y = .,
            by = c("cell_n")) %>%
  mutate(source = "polygons")

df_mbs_all <- bind_rows(df_mbs_vals %>% sample_n(10000),
                        #df_mbs_poly,
                        df_mbs_points)


ggplot(df_mbs_all %>%
         mutate(name.x = str_c("x: ", name.x),
                name.y = str_c("y: ", name.y)),
       aes(x = value.x, y = value.y, col = source)) +
  geom_point(size = 0.2) +
  facet_wrap(~name.x + name.y, scales="free")




