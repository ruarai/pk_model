bootstrap_out_path <- "output/update/bootstrap_outputs/"

bootstrap_files <- list.files(bootstrap_out_path,full.names=TRUE)


bootstraps <- unlist(lapply(bootstrap_files,
                     function(x){
                       readRDS(x)
                     }), recursive=FALSE)

saveRDS(bootstraps, file = "output/update/brt_model_list.Rds")