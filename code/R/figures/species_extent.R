
# load packages
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(seegSDM)

# load mask shape files
fascicularis <- shapefile('data/raw/reservoir//Mfascicularis.shp')
nemestrina <- shapefile('data/raw/reservoir/Mnemestrina.shp')
leonina <- shapefile('data/raw/reservoir/Mleonina.shp')
leucosphyrus <- shapefile('data/raw/reservoir/leucosphyrus_group.shp')

# combined monkey masks (places where at least one monkey is present)
monkeys <- gUnion(fascicularis, nemestrina)
#monkeys <- gUnion(monkeys, leonina)

# find areas where monkey and vectors masks overlap
res_vec <- gIntersection(monkeys, leucosphyrus)

# load template raster
template <- raster('data/clean/raster/SEAsia.tif')

# rasterize the res vec mask
res_vec_mask <- rasterize(res_vec, template)

# load unmasked risk map
pred <- raster('data/')

# mask pred by res vec mask
pred_masked <- mask(pred, res_vec_mask, updatevalue = 0)
#pred_masked <- mask(pred, res_vec_mask)

# save masked prediction
writeRaster(pred_masked,
            file='../knowlesi_parasite/output/MBS/SEAsia_masked',
            format='GTiff',
            overwrite=TRUE)

writeRaster(pred_masked,
            file='../../../mapping projects/malaria_knowlesi/R_outputs/human_disease/SE_Asia_prediction/May 2 2016/risk_map_masked.tif',
            format='GTiff',
            overwrite=TRUE)