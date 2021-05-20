
run_unique_name <- "replicate_FS_test"

model_stats <- do.call(rbind, 
                       lapply(list.files(paste0("output/update/model_stats/", run_unique_name)
                                         , full.names = TRUE),
                              read.csv))

model_stats <- model_stats[,-1]


print(paste0("AUC mean:", mean(model_stats$auc)))

print(paste0("RMSE mean:", mean(model_stats$rmse)))

library(Hmisc)

hist.data.frame(model_stats)



model_rel_inf <- do.call(rbind, 
                       lapply(list.files(paste0("output/update/model_rel_inf/", run_unique_name)
                                         , full.names = TRUE),
                              read.csv))

model_rel_inf <- model_rel_inf[,-1]

