# organise polygon shapefiles for plotting for exMBS

rm(list = ls())

# load packages
library(raster)
library(rgeos)
library(seegSDM)

# load admin shapefiles
admin1 <- shapefile("C:/Users/freyas/Dropbox/DPHIL/mapping/japanese_encephalitis/data/raster/raw/admin/admin2013_1.shp")
admin2 <- shapefile("C:/Users/freyas/Dropbox/DPHIL/mapping/japanese_encephalitis/data/raster/raw/admin/admin2013_2.shp")
para_ras1 <- raster('data/raw/raster/polygon_rasters/para_ras1.tif')
para_ras2 <- raster('data/raw/raster/polygon_rasters/para_ras2.tif')
knowlesi_range <- shapefile('../knowlesi_parasite/data/raw/raster/polygon_rasters/knowlesi_range.shp')

# ~~~~~~~~~~~~~
# sort polygons outside MBS

# load occurrence data with polygons
occ <- read.csv('../knowlesi_parasite/data/raw/occurrence/presence_absence_ex-MSB_confirmed.csv')

# ~~~~~~~~~~~~~~
# create relevant data subsets

# subset polygon data
occ <- occ[occ$Geometry_type == 'polygon',]

# subset into admin1 polygons 
admin1_polys <- occ[occ$Admin_level == 'Admin1',]

# subset into presence and absence
admin1_polys_pres <- admin1_polys[admin1_polys$Presence ==1,]
#admin1_polys_abs <- admin1_polys[admin1_polys$Presence ==0,]

# subset into admin2 polygons
admin2_polys <- occ[occ$Admin_level =='Admin2',]

# subset into presence and absence
admin2_polys_pres <- admin2_polys[admin2_polys$Presence ==1,]
admin2_polys_abs <- admin2_polys[admin2_polys$Presence ==0,]

# all of the para ras1 and ras2 polygons are presences
para_ras1_polys_pres <- occ[occ$Admin_level == 'para_ras1',]
para_ras2_polys_pres <- occ[occ$Admin_level == 'para_ras2',]

# all of these pk range polygons are absences
pk_range_polys_abs <- occ[occ$Admin_level == 'knowlesi_range',]

# ~~~~~~~~~~~~~~
# get gaul codes and subset admin layers

# get admin1 gaul codes
codes_admin1_pres <- admin1_polys_pres$Gaul_code

# subset admin1 layer
admin1_pk_pres <- admin1[admin1$GAUL_CODE %in% codes_admin1_pres,]

#codes_admin1_abs <- admin1_polys_abs$Gaul_code
#admin1_pk_abs <- admin1[admin1$GAUL_CODE %in% codes_admin1_abs,]

# get admin2 gaul codes and subset layer
# presences
codes_admin2_pres <- admin2_polys_pres$Gaul_code
admin2_pk_pres <- admin2[admin2$GAUL_CODE %in% codes_admin2_pres,]

# then absences
codes_admin2_abs <- admin2_polys_abs$Gaul_code
admin2_pk_abs <- admin2[admin2$GAUL_CODE %in% codes_admin2_abs,]

# saves files
shapefile(admin1_pk_pres, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin1_pk_pres.shp', 
          overwrite = TRUE)


#shapefile(admin1_pk_abs, 
#          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin1_pk_abs.shp', 
#          overwrite = TRUE)


shapefile(admin2_pk_pres, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin2_pk_pres.shp', 
          overwrite = TRUE)


shapefile(admin2_pk_abs, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin2_pk_abs.shp', 
          overwrite = TRUE)



# ~~~~~~~~~~~~~
# get codes for bespoke polygons 

# pk range absences already contains polygon boundaries
codes_pk_range_abs <- pk_range_polys_abs$Polygon_code
pk_range_abs <- knowlesi_range[knowlesi_range$GAUL_CODE %in% codes_pk_range_abs,] 

# save file
shapefile(pk_range_abs,
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/pk_range_abs.shp',
          overwrite=TRUE)

# get codes bespoke polygons in raster format
codes_para_ras1_pres <- para_ras1_polys_pres$Polygon_code

# para_ras1[!(para_ras1 %in% codes_para_ras1_pres)] <- NA  

# convert each polygon raster to a separate shapefile

# must delete files first due to bug in overwrite function when using windows
for (i in 1:length(codes_para_ras1_pres)) {
  
  # get code
  code <- codes_para_ras1_pres[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras1, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/exMBS confirmed shapefiles/ras1_pres_poly_',code), 
            overwrite = TRUE)
  
}

# do the same for para ras1
codes_para_ras2_pres <- para_ras2_polys_pres$Polygon_code

#para_ras2[!(para_ras2 %in% codes_para_ras2_pres)] <- NA

for (i in 1:length(codes_para_ras2_pres)) {
  
  # get code
  code <- codes_para_ras2_pres[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras2, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/exMBS confirmed shapefiles/ras2_pres_poly_',code), 
            overwrite = TRUE)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# organise polygon shapefiles for MBS

occ <- read.csv('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/model parasite data OCT2015_MBS_polys_ex_largepolys.csv')

# ~~~~~~~~~~~~~~
# create relevant data subsets

# subset into admin1 polygons and further subet into presence/absence
admin1_polys <- occ[occ$Admin_level == 'Admin1',]
admin1_polys_pres <- admin1_polys[admin1_polys$Presence ==1,]
admin1_polys_abs <- admin1_polys[admin1_polys$Presence ==0,]

# subset into admin2 polygons and further subset into presence/absence
admin2_polys <- occ[occ$Admin_level =='Admin2',]
admin2_polys_pres <- admin2_polys[admin2_polys$Presence ==1,]
admin2_polys_abs <- admin2_polys[admin2_polys$Presence ==0,]

# all of para ras1 and ras2 polygons are presence
para_ras1_polys <- occ[occ$Admin_level == 'para_ras1',]
para_ras1_polys_pres <- para_ras1_polys[para_ras1_polys$Presence ==1,]
para_ras1_polys_abs <- para_ras1_polys[para_ras1_polys$Presence ==0,]

para_ras2_polys <- occ[occ$Admin_level == 'para_ras2',]
para_ras2_polys_pres <- para_ras2_polys[para_ras2_polys$Presence ==1,]
para_ras2_polys_abs <- para_ras2_polys[para_ras2_polys$Presence ==0,]

# ~~~~~~~~~~~~~~
# get gaul codes and subset admin layers

# admin1 presences
codes_admin1_pres <- admin1_polys_pres$Gaul_code
admin1_pk_pres <- admin1[admin1$GAUL_CODE %in% codes_admin1_pres,]

# then absences
codes_admin1_abs <- admin1_polys_abs$Gaul_code
admin1_pk_abs <- admin1[admin1$GAUL_CODE %in% codes_admin1_abs,]

# admin2 presences
codes_admin2_pres <- admin2_polys_pres$Gaul_code
admin2_pk_pres <- admin2[admin2$GAUL_CODE %in% codes_admin2_pres,]

# then absences
codes_admin2_abs <- admin2_polys_abs$Gaul_code
admin2_pk_abs <- admin2[admin2$GAUL_CODE %in% codes_admin2_abs,]

# save files

shapefile(admin1_pk_pres, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin1_mbs_pres.shp', 
          overwrite = TRUE)

shapefile(admin1_pk_abs, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin1_mbs_abs.shp', 
          overwrite = TRUE)

shapefile(admin2_pk_pres, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin2_mbs_pres.shp', 
          overwrite = TRUE)


shapefile(admin2_pk_abs, 
          'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/admin2_mbs_abs.shp', 
          overwrite = TRUE)

# ~~~~~~~~~~~~~

# get codes for bespoke polygons
codes_para_ras1_pres <- para_ras1_polys_pres$Polygon_code

# convert each polygon raster to a separate shapefile
# must delete files first due to bug in overwrite function when using windows

for (i in 1:length(codes_para_ras1_pres)) {
  
  # get code
  code <- codes_para_ras1_pres[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras1, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/MBS confirmed shapefiles/ras1_pres_poly_',code), 
            overwrite = TRUE)
  
}

codes_para_ras1_abs <- para_ras1_polys_abs$Polygon_code

for (i in 1:length(codes_para_ras1_abs)) {
  
  # get code
  code <- codes_para_ras1_abs[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras1, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/MBS confirmed shapefiles/ras1_abs_poly_',code), 
            overwrite = TRUE)
  
}

codes_para_ras2_pres <- para_ras2_polys_pres$Polygon_code

for (i in 1:length(codes_para_ras2_pres)) {
  
  # get code
  code <- codes_para_ras2_pres[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras2, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/MBS confirmed shapefiles/ras2_pres_poly_',code), 
            overwrite = TRUE)
  
}

codes_para_ras2_abs <- para_ras2_polys_abs$Polygon_code

for (i in 1:length(codes_para_ras2_abs)) {
  
  # get code
  code <- codes_para_ras2_abs[[i]]
  
  # convert pixels containing this code to a shapefile 
  poly <- rasterToPolygons(para_ras2, fun = function(x){x==code}, dissolve=TRUE)
  
  # and save
  shapefile(poly,
            paste0('C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/MBS confirmed shapefiles/ras2_abs_poly_',code), 
            overwrite = TRUE)
  
}




# writeRaster(para_ras1,
#             file = 'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/para_ras1__mbs_pres',
#             format = 'GTiff',
#             overwrite = TRUE)
# 
# writeRaster(para_ras1b,
#             file = 'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/para_ras1_mbs_abs',
#             format = 'GTiff',
#             overwrite = TRUE)
# 
# writeRaster(para_ras2,
#             file = 'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/para_ras2_mbs_pres',
#             format = 'GTiff',
#             overwrite = TRUE)
# 
# writeRaster(para_ras2b,
#             file = 'C:/Users/freyas/Dropbox/DPHIL/mapping projects/malaria_knowlesi/plos_NTDs/figures/SI/para_ras2_mbs_abs',
#             format = 'GTiff',
#             overwrite = TRUE)