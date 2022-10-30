
library(tidyverse)

final_1_pred_matrix <- read_rds("/data/gpfs/projects/punim1449/knowlesi_ruarai/output/update/final_1_pred_matrix.Rds")



full_matrix <- final_1_pred_matrix %>%
  as.matrix() %>%
  as_data_frame()




x <- final_1_pred_matrix[,1:10]


full_tbl <- full_matrix %>%
  as_tibble()


feather::write_feather(
  full_tbl,
  "output/update/final_1_export.feather"
)




