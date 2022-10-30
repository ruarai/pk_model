

stats_files <- list.files(
  "output/update/model_stats/final_1/",
  pattern  = "testing_stats",
  full.names = TRUE
)

stats <- map_dfr(stats_files, function(x) { read_csv(x)})

# Mean
mean(stats$auc)

# SE
sd(stats$auc) / sqrt(nrow(stats))




stats_files_B <- list.files(
  "output/update/model_stats/final_1/",
  pattern  = "\\d_stats",
  full.names = TRUE
)

stats_B <- map_dfr(stats_files_B, function(x) { read_csv(x)})
