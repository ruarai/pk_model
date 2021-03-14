setwd("C:/Users/ruarai/Dropbox/ZOOMAL - Spatial Modelling/model_update")


covs_current <- brick("data/clean/raster/covs_current.grd")


covs_data <- as.data.frame(values(covs_current)) %>%
  drop_na()

discrete_vars <- c("forest_intact",
                   "open_shrublands",
                   "woody_savannas",
                   "savannas",
                   "grasslands",
                   "permanent_wetlands",
                   "croplands",
                   "cropland_natural_vegetation_mosaic")



covs_continuous <- covs_data %>%
  dplyr::select(-all_of(discrete_vars))

covariate_MI <- read.csv("data/raw/covariate_dependence/out.csv")
rownames(covariate_MI) <- colnames(covs_continuous)

MI <- covariate_MI[,2:12]
BCMI <- covariate_MI[,13:23]
Z <- covariate_MI[,24:34]

colnames(MI) <- colnames(covs_continuous)
colnames(BCMI) <- colnames(covs_continuous)
colnames(Z) <- colnames(covs_continuous)

MI <- MI %>% add_rownames() %>% pivot_longer(cols = 2:last_col())
BCMI <- BCMI %>% add_rownames() %>% pivot_longer(cols = 2:last_col())
Z <- Z %>% add_rownames() %>% pivot_longer(cols = 2:last_col())
COR <- as.data.frame(cor(covs_continuous)) %>% add_rownames() %>% pivot_longer(cols = 2:last_col())

library(ggplot2)

ggplot(COR, aes(x = rowname, y = name, fill=abs(value))) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position = 'none')


ggplot(MI, aes(x = rowname, y = name, fill=value)) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_continuous(trans = "log",type = "viridis") +
  theme(legend.position = 'none')





