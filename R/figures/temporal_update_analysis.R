
library(tidyverse)

library(scales)

data_FS <- read.csv("data/clean/occurrence/pk_present/MBS_FS_B_2005-2014.csv")# %>%
  #filter(Geometry_type == "point")

data_MT <- bind_rows(read.csv("data/clean/occurrence/pk_present/MBS_MT_point_2007-2018.csv"),
                     read.csv("data/clean/occurrence/pk_present/MBS_MT_polygon_2007-2018.csv"))


data_FS$source <- "FS"
data_MT$source <- "MT"

data_sourced <- bind_rows(data_FS,
                          data_MT)

data_all_2012 <- read.csv("output/update/various/temporal_update_analysis/data_all_2012.csv")
data_all_2019 <- read.csv("output/update/various/temporal_update_analysis/data_all_2019.csv")

data_all_2012 <- data_all_2012 %>%
  mutate(year_max = "2012", row_n = row_number())
data_all_2019 <- data_all_2019 %>%
  mutate(year_max = "2019", row_n = row_number())

data_all <- bind_rows(data_all_2012, data_all_2019)



data_all_joined <- data_all %>%
  left_join(data_sourced, by = c("Longitude", "Latitude", "Year", "Host", "Geometry_type"))

data_all_joined <- bind_cols(data_all_joined %>% select("year_max", "row_n", "source"),
                             data_all_joined[,11:30])

data_all_joined_l <- data_all_joined %>%
  pivot_longer(cols = -c(year_max, row_n, source))

long_2012 <- data_all_joined_l %>% filter(year_max == "2012")
long_2019 <- data_all_joined_l %>% filter(year_max == "2019")

data_all_rejoined <- long_2012 %>%
  left_join(long_2019, by = c("name", "row_n", "source"), suffix = c("_2012", '_2019')) %>%
  select(c("name", "row_n", "value_2012", "value_2019", "source")) %>%
  mutate(value_diff = value_2019 - value_2012)

with_change <- data_all_rejoined %>%
  filter(abs(value_diff) > 0.5) %>% pull(name) %>% unique()


data_all_sampled <- data_all_rejoined %>%
  filter(name %in% with_change) %>%
  group_by(name) %>%
  sample_n(10000)

data_all_sampled %>%
  ggplot() +
  geom_point(aes(x = value_2012, y = value_2019, col = source),
             size=0.5) +
  facet_wrap(~name, scales="free")



data_all_rejoined %>%
  filter(abs(value_diff) > 0.5) %>%
  ggplot() +
  geom_histogram(aes(x = value_diff, fill = source),
                 position = "stack") +
  facet_wrap(~name, scales="free") +
  #scale_y_continuous(trans = "log10") +
  ylab("Count") + xlab("Value increase/decrease with 2019 temporal dataset")




