
# Inclusive max/min
year_min <- 2001
year_max <- 2019


library(tidyverse)

library(scales)
library(ggforce)


data_FS <- read.csv("data/clean/occurrence/pk_present/MBS_FS_B_2005-2014.csv") %>%
  filter(Geometry_type == "point")

data_MT <- read.csv("data/clean/occurrence/pk_present/MBS_MT_point_2007-2018.csv")

data_FS$source <- "FS"
data_MT$source <- "MT"

data_all <- rbind(data_FS, data_MT)
data_all_with_covs <- read.csv("data/clean/occurrence/data_all.csv")

data_all_joined <- data_all %>%
  left_join(data_all_with_covs, by = c("Longitude", "Latitude", "Year", "Host", "Geometry_type"))

data_all_scaled <- bind_cols(data_all_joined %>% select("source"),
                             sapply(data_all_joined[,13:ncol(data_all_joined)], rescale) %>%
                               data.frame())

training_samples <- data_all_scaled %>%
  pivot_longer(cols = -c(source))



ggplot() +
  geom_histogram(data = training_samples %>% filter(source == "FS"),
                 mapping = aes(x = value, fill = source, y = ..count..), size=0.2, bins=50) +
  geom_histogram(data = training_samples %>% filter(source == "MT"),
                 mapping = aes(x = value, fill = source, y = -..count..), size=0.2, bins=50) +
  facet_grid(rows=vars(name), scales="free") +
  theme_bw() +
  theme(strip.text.y.right = element_text(angle = 0))
