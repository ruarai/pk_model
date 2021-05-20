
# clear workspace
rm(list=ls())

library(raster)
library(seegSDM)

# prepare culex tritaeniorhynchus covariate layer

cx_tri <- raster('cx_tri')

# load template raster
template <- raster('china_covs.grd') * 0

# add projection info
projection(cx_tri) <- projection(template)

cx_tri <- crop(cx_tri, template)


#cx_tri <- projectRaster(projection(cx_tri), projection(template))

# log transform
cx_tri_log <- log(cx_tri)



# resample
cx_tri_resampled <- resample(cx_tri_log,
                             template,
                             method='bilinear')

# transform back
cx_tri_final <- exp(cx_tri_resampled)

cx_tri_final <- mask(cx_tri_final, template)

plot(log(cx_tri_final))

# output resampled raster
writeRaster(cx_tri_final,
            file='cx_tri',
            format='GTiff',
            overwrite=TRUE)

# prepare pig layer 
pigs <- raster('Glb_Pigs_CC2006_AD.tif')
pigs <- crop(pigs, template)

# resolution of the template and pigs layer are very different so aggregate first
pigs_agg <- aggregate(pigs, 5, FUN = sum)

# transform
# log (1+ x) - because contains zeroes which will end up being -infinity
l_pigs_agg <- log1p(pigs_agg)


pigs_resampled <- resample(l_pigs_agg,
                           template,
                           method='bilinear')

# expm1(x) computes exp(x)-1
pigs_resampled <- expm1(pigs_resampled)

pigs_resampled <- mask(pigs_resampled, template)

plot(pigs_resampled)



plot(log1p((pigs_resampled / maxValue(pigs_resampled)) * cx_tri_final))
