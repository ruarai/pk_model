library(raster)
library(tidyverse)

occ_files <- c("MBS_FS_B_2005-2014.csv",
               "MBS_MT_point_2007-2018.csv",
               "MBS_MT_polygon_2007-2018.csv")

occ_files <- str_c("data/clean/occurrence/pk_present/", occ_files)

test <- read.csv(occ_files[1])



occ_data <- lapply(occ_files, read.csv)


# We need to make sure that unique ids remain unique across different datasets:

max_uids <- sapply(occ_data, function(x) length(unique(x$Unique_ID)))

occ_data <- lapply(1:length(occ_data), function(i) {
  uid_offset <- sum(max_uids[0:(i-1)])
  
  x <- occ_data[[i]]
  source <- occ_files[i]
  # Clean each dataframe so that unique id is of form 1, 2, 3 ..
  # And then add an offset of the previous max unique id
  x %>% 
    mutate(Original_Unique_ID = Unique_ID,
           Source = source) %>%
    mutate(Unique_ID = match(Unique_ID, unique(Unique_ID)) + uid_offset) 
})

# Then finally merge
occ_data <- bind_rows(occ_data)

# Should be strictly increasing:
plot(occ_data$Unique_ID, pch='.')




point_data <- occ_data %>%
  filter(Geometry_type == 'point')

poly_data <- occ_data %>%
  filter(Geometry_type == 'polygon')


mbs_mask <- raster("data/clean/raster/mbs_mask")
mbs_mask[mbs_mask == 1] <- 0

plot_poly_overlap <- function(poly_data){
  cell_ns <- raster::cellFromXY(mbs_mask, poly_data %>% select("Longitude", "Latitude"))
  mbs_vals <- getValues(mbs_mask)
  cell_table <- table(cell_ns)
  
  mbs_vals[as.numeric(names(cell_table))] <- as.vector(cell_table)
  
  
  plot(setValues(mbs_mask, mbs_vals))
}



polygon_size <- poly_data %>%
  group_by(Unique_ID) %>%
  summarise(cell_n = n())

ggplot(polygon_size) +
  geom_histogram(aes(x = cell_n, y = (..count..)/sum(..count..) * 3),
                 bins = 100) + 
  scale_y_continuous(name = "Empirical CDF", 
                     sec.axis = sec_axis(~.*(1/3), name="Density")) +
  stat_ecdf(aes(x = cell_n), color="red") +
  ggtitle("Polygon Size Distribution",
          "Excluding polygons with >1000 cells") 

ggplot(polygon_size) +
  geom_histogram(aes(x = cell_n, y = (..count..)/sum(..count..) * 10),
                 bins = 600) +
  coord_cartesian(xlim = c(0,1000)) + 
  scale_y_continuous(name = "Empirical CDF", 
                     sec.axis = sec_axis(~.*0.1, name="Density")) +
  stat_ecdf(aes(x = cell_n), color="red") +
  ggtitle("Polygon Size Distribution",
          "Excluding polygons with >1000 cells") 

polygons_include <- polygon_size %>%
  filter(cell_n <= 1000)


poly_data_thinned <- poly_data %>%
  filter(Unique_ID %in% polygons_include$Unique_ID)


plot_poly_overlap(poly_data_thinned)
plot(SpatialPoints(point_data %>% select(Longitude, Latitude)),
     add=TRUE, pch=16, col = 'blue', cex = 0.3)
title("Occurrence data")


occ_data_thinned <- bind_rows(point_data,
                              poly_data_thinned)
occ_data_thinned <- occ_data_thinned %>%
  mutate(PA = 1,
         wt = 1)

write.csv(occ_data_thinned,
          "data/clean/occurrence/pk_present/ALL_occ_thinned.csv",
          row.names = FALSE)


