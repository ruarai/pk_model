
library(raster)

library(tidyverse)

library(scales)

data_FS <- read.csv("data/clean/occurrence/pk_present/MBS_FS_B_2005-2014.csv")

data_MT <- bind_rows(read.csv("data/clean/occurrence/pk_present/MBS_MT_point_2007-2018.csv"),
                     read.csv("data/clean/occurrence/pk_present/MBS_MT_polygon_2007-2018.csv"))


data_FS$source <- "FS"
data_MT$source <- "MT"

data_sourced <- bind_rows(data_FS,
                          data_MT)


now_temporal <- c("human_pop",
                  "TCW_SD",
                  "TCW_mean",
                  "TCB_SD")



covs_temporal <- brick('data/clean/raster_updated/temporal_MBS.grd')
covs_nontemporal <- brick('data/clean/raster_updated/nontemporal_MBS.grd')



covs_current <- brick('data/clean/raster_updated/prediction_MBS.grd')


data_covs <- raster::extract(covs_current, data_sourced[, c('Longitude', 'Latitude')])
data_covs[] <- NA



year_min <- 2001
year_max <- 2019
data_sourced <- data_sourced %>%
  mutate(Year_Constrained = pmin(pmax(Year, year_min), year_max))

for (year in year_min:year_max) {
  
  # Get temporal covariates for our current year
  covs_year <- covs_temporal[[str_which(names(covs_temporal), str_c("_", year))]]
  
  # Remove year tags from our covariates
  names(covs_year) <- str_replace(names(covs_year), str_c("_", year), "")
  
  # Add non-temporal covariates
  covs_year <- addLayer(covs_year, covs_nontemporal)
  
  long_lats_year <- data_sourced %>%
    filter(Year_Constrained == year) %>%
    select(Longitude, Latitude)
  
  # extract data
  covs_year_extract <- raster::extract(covs_year, long_lats_year)
  
  # check they're all there
  stopifnot(all(colnames(data_covs) %in% colnames(covs_year_extract)))
  
  # match up the column names so they're in the right order
  match <- match(colnames(data_covs), colnames(covs_year_extract))
  
  data_covs[data_sourced$Year_Constrained == year,] <- covs_year_extract[, match]
}

data_covs_temporal <- data_covs %>% data.frame()
data_covs_temporal$source <- data_sourced$source
data_covs_temporal$year <- data_sourced$Year_Constrained

data_covs_nontemporal <-  raster::extract(covs_current, data_sourced[, c('Longitude', 'Latitude')])
data_covs_nontemporal[] <- NA


covs_2012 <- covs_temporal[[str_which(names(covs_temporal), "_2012")]]
names(covs_2012) <- str_replace(names(covs_2012), "_2012", "")

long_lats_all <- data_sourced %>%
  select(Longitude, Latitude)

covs_2012_extract <- raster::extract(covs_2012, long_lats_all)

# match up the column names so they're in the right order
match <- match(colnames(data_covs_nontemporal), colnames(covs_2012_extract))

data_covs_nontemporal[,] <- covs_2012_extract[, match]

data_covs_nontemporal <- data_covs_nontemporal %>% data.frame() 

data_covs_nontemporal$source <- data_sourced$source
data_covs_nontemporal$year <- data_sourced$Year_Constrained






data_covs_temporal <- data_covs_temporal %>%
  mutate(row_n = row_number(),
         type = "temporal")

data_covs_nontemporal <- data_covs_nontemporal %>%
  mutate(row_n = row_number(),
         type = "nontemporal")

data_all <- bind_rows(data_covs_temporal, data_covs_nontemporal) %>%
  pivot_longer(cols = -c(type, row_n, source, year)) %>%
  left_join(x = filter(., type == "temporal"),
            y = filter(., type == "nontemporal"),
            by = c("row_n", "name", "source", "year"),
            suffix = c("_temporal", "_nontemporal")) %>%
  mutate(value_changed = value_temporal - value_nontemporal)


data_sampled <- data_all %>%
  group_by(name) %>%
  sample_n(10000)

ggplot(data_sampled) +
  geom_point(aes(y = value_temporal, x = value_nontemporal, col = source),
             size = 0.5) +
  facet_wrap(~name + source, scales="free") +
  theme_bw()

ggplot(data_sampled) +
  geom_jitter(aes(x = year, y = value_changed, col = source),
              size=0.2, width=0.5, height=0, alpha=0.2)+
  facet_wrap(~name + source, scales="free") +
  theme_bw()

ggplot(data_all) +
  geom_vline(xintercept = 2012) + geom_hline(yintercept = 0) +
  geom_violin(aes(y = value_changed, x = year, col = source, group = interaction(year, source))) +
  facet_wrap(~name, scales="free_y") +
  theme_bw() +
  ylab("Change in value")

ggplot(filter(data_all)) +
  geom_histogram(aes(x = value_changed, fill = source), position='stack') +
  geom_vline(xintercept = 0, col ='blue') +
  facet_wrap(~name, scales="free") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  ylab("Density (log10)")











