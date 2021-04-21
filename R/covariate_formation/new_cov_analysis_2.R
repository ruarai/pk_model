

library(raster)

library(tidyverse)

library(infotheo)
library(colorspace)

setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")

all_covariates <- brick("data/clean/raster_updated/prediction_SEA.grd")

risk <- raster("data/clean/raster/SEAsia_FS.tif")

cov_names <- names(all_covariates)

new_covs <- cov_names[str_detect(cov_names,"SEA_")]
old_covs <- cov_names[!str_detect(cov_names,"SEA_")]


cov_values <- getValues(all_covariates)
risk_values <- getValues(risk)

point_samples <- sample(which(!is.na(rowSums(cov_values))), size = 10000)


cov_subset <- cov_values[point_samples,]
A_risk_subset <- risk_values[point_samples]

cov_subset <- cbind(cov_subset, A_risk_subset)

cov_cor <- cor(cov_subset)


cov_plot_data <- cov_cor %>%
  data.frame() %>%
  mutate(row_name = rownames(.)) %>%
  pivot_longer(cols = -row_name) %>%
  rename(A = row_name, B = name)

ggplot(cov_plot_data) +
  geom_tile(aes(x=A,y=B, fill = value)) +
  scale_fill_continuous_diverging(palette = "Purple-Green")+
  theme(axis.text.x = element_text(angle = -45,hjust=0))


library(corrr)

cov_AMI <- cov_subset %>%
  data.frame() %>%
  discretize() %>%
  colpair_map(AMI)


cov_mi_plot_data <- cov_AMI %>%
  data.frame() %>%
  pivot_longer(cols = -term) %>%
  rename(A = term, B = name)

ggplot(cov_mi_plot_data) +
  geom_tile(aes(x=A,y=B, fill = value)) +
  scale_fill_continuous_diverging(palette = "Purple-Green")+
  theme(axis.text.x = element_text(angle = -45,hjust=0))

