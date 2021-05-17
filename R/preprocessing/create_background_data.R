

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

# Map libraries
library(raster)
library(seegSDM)

source('code_ruarai/R/functions_parasite.R')

human_pop <- raster("data/clean/raster_updated/human_pop_MBS")

mbs_mask <- raster("data/clean/raster/mbs_mask")



occ_data <- read.csv("data/clean/occurrence/pk_present/ALL_occ_thinned.csv")

distinct_samples <- bind_rows(occ_data %>% filter(Geometry_type == 'point'),
                              occ_data %>% filter(Geometry_type == 'polygon') %>% 
                                distinct(Unique_ID, .keep_all = T))

# Determine the number of each host
host_counts <- data.frame(host_name = c("human", "mosquito","monkey"), counts = numeric(3), points = numeric(3))
host_counts <- host_counts %>%
  rowwise() %>%
  mutate(counts = distinct_samples %>%
           filter(Host == host_name) %>%
           nrow())

background_point_n <- 6000
total_presence_n <- sum(host_counts$counts)

host_counts <- host_counts %>%
  rowwise() %>%
  mutate(points = round(counts / total_presence_n * background_point_n))


fascicularis <- read.csv('data/clean/occurrence/fascicularis_presence.csv')
nemestrina <- read.csv('data/clean/occurrence/nemestrina_presence.csv')
monkey_bias <- read.csv('data/clean/occurrence/mammals-bias.csv')

# Remove NA columns
fascicularis <- fascicularis %>%
  select(-c(9:18))

monkey_bias <- monkey_bias %>%
  rename(Latitude = decimalLatitude,
         Longitude = decimalLongitude,
         Year = year)

monkey_bias$Geometry_type <- 'point'

fascicularis <- fascicularis %>%
  filter(Presence == 1)
nemestrina <- nemestrina %>%
  filter(Presence == 1)

fascicularis <- fascicularis %>%
  rename(Geometry_type = Geometry_t)


monkey_data <- bind_rows(fascicularis, nemestrina, monkey_bias) %>%
  select(Longitude, Latitude, Year, Geometry_type)

monkey_data <- insideMask(monkey_data, human_pop)

monkey_bg <- monkey_data[sample(1:nrow(monkey_data),
                                host_counts %>% filter(host_name == "monkey") %>% pull(points), # yuk
                                replace=FALSE),]
monkey_bg$Host <- 'monkey'



human_bg <- bgSample (human_pop,
                      n=host_counts %>% filter(host_name == "human") %>% pull(points),
                      prob=TRUE, replace= TRUE, spatial=FALSE)

vector_bg <- bgSample (human_pop,
                       n=host_counts %>% filter(host_name == "mosquito") %>% pull(points),
                       prob=TRUE, replace=TRUE, spatial=FALSE)


year_sample_dist <- distinct_samples$Year

# Function to manipulate out points data into usable form
points_to_df <- function(points, host){
  data.frame(points) %>%
    mutate(Year = sample(year_sample_dist, size = nrow(.), replace=TRUE),
           Geometry_type = 'point',
           Host = host) %>%
    rename(Longitude = x,
           Latitude = y)
}


background_data <- bind_rows(points_to_df(human_bg, "human"),
                             points_to_df(vector_bg, "mosquito"),
                             monkey_bg)
background_data$Unique_ID <- NA



bg_all <- background_data %>%
  select(Unique_ID, Longitude, Latitude, Year, Geometry_type, Host) %>%
  mutate(PA = 0,
         wt = 1)


mbs_mask <- raster("data/clean/raster/mbs_mask")
mbs_mask[mbs_mask == 1] <- 0

plot_point_overlap <- function(point_data){
  cell_ns <- raster::cellFromXY(mbs_mask, point_data %>% select("Longitude", "Latitude"))
  mbs_vals <- getValues(mbs_mask)
  cell_table <- table(cell_ns)
  
  mbs_vals[as.numeric(names(cell_table))] <- as.vector(cell_table)
  
  plot(mbs_mask, col = 'gray80')
  
  
  
  title("Pseudo-absence/background data")
  plot(SpatialPoints(point_data %>% select(Longitude, Latitude)),
       add=TRUE, pch='.', col = rgb(0,0,1,0.5), cex = 0.5)
}

plot_point_overlap(bg_all %>%
                     filter(Host == "human" | Host == "mosquito"))




write.csv(bg_all, "data/clean/occurrence/ALL_background_points.csv", row.names = FALSE)

