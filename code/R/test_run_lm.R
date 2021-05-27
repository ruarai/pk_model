

library(tidyverse)


data_all <- read.csv("data/clean/occurrence/data_all.csv")
data_all$PA <- factor(data_all$PA)

host_index <- match("Host_species", names(data_all))

data_vals <- bind_cols(
  data_all %>% dplyr::select(PA),
  data_all %>% dplyr::select(all_of(host_index:ncol(data_all)))
)

colors_risk <- colorRampPalette(c("#55843b", "#a4be79","#ffffbf", "#921d67", "#6c0043"))(100)

sea_blank <- raster('data/clean/raster/SEAsia_extent.grd')
pred_raster <- getValues(brick("data/clean/raster_updated/prediction_SEA"))

pred_X <- bind_cols(
  data.frame(Host_species = 3),
  as.data.frame(pred_raster)
)

narows <- which(is.na(rowSums(pred_X)))


library(caret)


train_index = createDataPartition(data_vals$PA, p = 0.75, list = FALSE)
training_data = data_vals[train_index, ]
testing_data = data_vals[-train_index, ]

default_glm_mod = train(
  form = PA ~ .,
  data = training_data,
  trControl = trainControl(method = "cv", number = 5),
  method = "gamSpline",
  family = "binomial"
)

mean(testing_data$PA == predict(default_glm_mod, newdata = testing_data))


risk_pred <- predict(default_glm_mod, newdata = pred_X, type='prob')

allvals <- rep(NA, times = ncell(sea_blank))
allvals[-narows] <- risk_pred[,2]

plot(setValues(sea_blank, allvals),
     zlim = c(0, 1),
     col = colors_risk)



