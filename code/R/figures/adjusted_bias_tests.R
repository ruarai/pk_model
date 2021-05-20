
# Map libraries
library(raster)
library(seegSDM)


highlight_extent <- c(108,120,-1,11)


access_healthcare <- raster("data/raw/covariate_production/gee_covs/SEA_access_healthcare.tif")
max_access <- max(getValues(access_healthcare),na.rm=TRUE)

access_healthcare <- 1 - access_healthcare / max_access
plot(access_healthcare, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])


old_covs <- brick("data/raw/covariate_production/nontemporal_final/old_covs")

nemestrina <- old_covs[["nemestrina"]]
fascicularis <- old_covs[["fascicularis"]]
leucosphyrus <- old_covs[["leucosphyrus_group"]]

human_pop <- old_covs[["human_pop"]]




plot(nemestrina, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])
plot(fascicularis, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])
plot(leucosphyrus, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])

prior_risk <- (fascicularis + nemestrina + leucosphyrus) / 3


plot(prior_risk, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])


sampling_bias <- access_healthcare * prior_risk


plot(sampling_bias, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])
plot(access_healthcare, xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])
plot(log(human_pop), xlim=highlight_extent[1:2], ylim=highlight_extent[3:4])


