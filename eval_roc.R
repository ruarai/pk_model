# Map libraries
library(raster)
library(tidyverse)

source("code/R/figures/maps_common.R")

test_points <- read.csv("data/clean/occurrence/testing/test_points.csv")
stopifnot(all(test_points$PA == 1))


test_poly_data <- read.csv("data/clean/occurrence/testing/test_poly.csv")

test_poly_shape <- readRDS("data/clean/occurrence/testing/poly_shapes.Rds")
rownames(test_poly_data) <- names(test_poly_shape)

test_poly_sdf <- SpatialPolygonsDataFrame(test_poly_shape, test_poly_data)
test_poly_sdf$PA <- factor(test_poly_sdf$PA)


ggplot() +
  geom_sf(data = SEA_simple,
          col='grey80',
          size = 0.2) +
  
  geom_sf(data = MBS_simple,
          fill = 'grey40',
          col = rgb(0,0,0,0)) +
  
  geom_sf(data = sf::st_as_sf(test_poly_sdf),
          mapping = aes(fill = PA),
          color=rgb(1,1,1,0.3),
          size = 0.2) +
  
  geom_blank(data = tibble(Nodata = "2"),
             mapping = aes(fill = Nodata)) +
  
  scale_fill_manual(values = c("0" = "#a3be7a", "1" = "#d79da0", "2" = "grey90"),
                    labels = c("Absence polygons", "Presence polygons", "No data"),
                    name = NULL) +
  
  coord_sf(xlim = extent(sea_mask)[1:2],
           ylim = extent(sea_mask)[3:4],
           expand=FALSE,
           datum = NA) +
  
  geom_point(data = test_points,
             aes(x = Longitude, y = Latitude, color = 'A'),
             size = 0.8) +
  
  scale_color_manual(values = c("A" = "#951d66"),
                     labels = "Presence points",
                     name = NULL) +
  
  theme(panel.background = element_rect(fill='white'),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.8,0.7),
        legend.direction = "vertical")




risk_mat <- read_rds("output/update/final_1_pred_matrix.Rds")
blank_seasia <- raster('data/clean/raster/SEAsia_extent.grd')

blank_seasia[] <- NA

test_points_spatial <- SpatialPoints(test_points[,c("Longitude", "Latitude")])

ix_seasia <- setValues(blank_seasia,1:ncell(blank_seasia)) 

points_ix <- raster::extract(ix_seasia,
                             test_points_spatial)

poly_ix <- raster::extract(ix_seasia,
                           test_poly_sdf)

testing_data <-
  
  map_dfr(
  1:nrow(test_poly_sdf),
  function(i) {
    if(length(poly_ix[[i]]) == 0) {
      return(tibble())
    }
    
    tibble(ix = poly_ix[[i]], PA = test_poly_data$PA[i])
    }
  ) %>%
  bind_rows(tibble(ix = points_ix, PA = 1)) %>%
  
  group_by(ix) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  
  arrange(ix) %>%
  
  mutate(PA_fct = factor(PA))





true_PA_bool <- testing_data$PA == 1

results <- map(
  1:100,
  function(ix_subset) {
    preds_at_interest <- risk_mat[ix_subset, testing_data$ix]
    
    res <- do.call(rbind, map(seq(0, 1, by = 0.015), function(threshold) {
      n_true_pos <- sum((preds_at_interest > threshold) & true_PA_bool)
      n_false_pos <- sum((preds_at_interest > threshold) & !true_PA_bool)
      
      
      c("fpr" = sum(n_false_pos) / sum(!true_PA_bool),
        "tpr" = sum(n_true_pos) / sum(true_PA_bool))
    }))
    
    rbind(
      c(0,0),
      res,
      c(1,1)
    )
  }
)

plot(results[[1]], type = "l")


for(i in 2:100) {
  lines(results[[i]])
}

sapply(1:100, function(i) integrate(approxfun(results[[i]][,1], results[[i]][,2]), 0, 1)$val) %>% mean()

  