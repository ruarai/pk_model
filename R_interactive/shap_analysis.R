

setwd("/data/gpfs/projects/punim1449/knowlesi_ruarai")

library(raster)

library(iml)

library(tidyverse)

library(cowplot)

run_unique_name <- "with_only_temporal_update"

m_data <- readRDS(paste0("output/update/bootstrap_outputs/",run_unique_name,"/1_brt_model_list.Rds"))

m_gbm <- m_data[[1]]

data_for_model <- readRDS(paste0("output/update/bootstrap_inputs/",run_unique_name,"/1_brt_data_list.Rds"))
data_for_model <- data_for_model[[1]]


X <- data_for_model[,10:ncol(data_for_model)]

model = Predictor$new(m_gbm$model, data = X, y = data_for_model$PA)

auc_error = function(actual, predicted) 1 - Metrics::auc(actual, predicted)

imp = FeatureImp$new(model, loss = auc_error)
plot(imp)


sea_pred_covs <- brick("data/clean/raster_updated/prediction_SEA")




line_of_interest <- tribble(~x, ~y,
                            99.16, 8.40,
                            103.98, 1.69)

library(sp)
line_sp <- sp::SpatialLines(list(sp::Lines(list(sp::Line(line_of_interest)), ID = 1)),
                            proj4string = crs(sea_pred_covs))


line_data <- raster::extract(sea_pred_covs, line_sp)[[1]]

line_pred <- as.data.frame(line_data)
line_pred$Host_species <- 3

line_pred <- line_pred[,match(names(X), names(line_pred))]


#shap <- readRDS("~/shap.Rds")


line_shap <- lapply(1:nrow(line_pred), function(i){
  if(i %% 10 == 0){
    print(i)
  }
  
  shap$explain(line_pred[i,])
  
  shap$results
})


line_shap_all <- bind_rows(line_shap)

line_shap_all <- line_shap_all %>%
  mutate(index = rep(1:(nrow(.) / 21), each = 21)) %>%
  mutate(feature_val = str_extract(feature.value, "(?<==).+$") %>%
           as.numeric())

line_shap_all$feature_fct <- factor(line_shap_all$feature,
                                    levels = imp$results$feature)
  
line_shap_long <- line_shap_all %>%
  pivot_longer(cols = c(feature_val, phi))

p_list <- list()

for(feature_i in imp$results$feature){
  p1 <- ggplot(data = line_shap_long %>% filter(feature == feature_i),
         mapping = aes(x = index, y = value)) +
    geom_line() +
    #geom_blank(data = tribble(~index, ~value, ~name,0,2,'phi',0,-2,'phi')) +
    facet_grid(rows = vars(name),
               scales = "free_y") +
    xlab("") + ylab("") +
    ggtitle(paste(feature_i, imp$results %>% filter(feature == feature_i) %>% pull(importance) %>% round(2))) +
    theme(plot.margin = margin())
  
  p_list <- c(p_list, list(p1))
}


plot_grid(plotlist = p_list)  




ggplot(line_shap_all,
       mapping = aes(x = feature_val, y = phi)) +
  geom_point(size=0.1) +
  facet_wrap(~feature_fct, scales = "free")









