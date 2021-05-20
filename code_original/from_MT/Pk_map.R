rm(list = ls())
library(raster)
library(rgeos)
library(rgdal)
library(dismo)
library(png)
library(sf)

dat <- read.csv("data/raw/occurrence/Pk_merged_MT.csv", header = TRUE, stringsAsFactors = FALSE)
data <-subset(dat, dat$Inclusion=="1") 
pres_data <- subset(data, data$Presence=="1")

poly_data <- pres_data[pres_data$Geometry_type=="polygon",]
point_data <- pres_data[pres_data$Geometry_type=="point",]

poly_data1 <- poly_data[poly_data$Admin_level=="1",]
poly_data2 <- poly_data[poly_data$Admin_level=="2",]
poly_data3 <- poly_data[poly_data$Admin_level=="3",]
# we need to reclassify these 3rd level's into 2nd level as there's no 3rd level shapes,
# can do by searching for 2nd level shapes that contain these 3rd level regions
poly_dataC <- poly_data[poly_data$Admin_level=='C',]
#these will be made into custom polygons (n=3) - all human, no monkey or mosquito

hpoly_data1 <- poly_data1[poly_data1$Host=='human',]
mkpoly_data1 <- poly_data1[poly_data1$Host=='macaque',]
mspoly_data1 <- poly_data1[poly_data1$Host=='mosquito',] #empty

hpoly_data2 <- poly_data2[poly_data2$Host=='human',]
mkpoly_data2 <- poly_data2[poly_data2$Host=='macaque',]
mspoly_data2 <- poly_data2[poly_data2$Host=='mosquito',]

hpoly_data3 <- poly_data3[poly_data3$Host=='human',]
mkpoly_data3 <- poly_data3[poly_data3$Host=='macaque',]

admin0_shp <- shapefile('data/raw/gadm_maps/gadm36_0.shp')
admin1_shp <- shapefile('data/raw/gadm_maps/gadm36_1.shp')
admin2_shp <- shapefile('data/raw/gadm_maps/gadm36_2.shp')

countries <- c('MYS', 'IDN', 'LAO', 'MMR', 'PHL', 'THA', 'VNM', 'KHM')
countries2 <- c('IND', 'MYS', 'IDN', 'LAO', 'MMR', 'PHL', 'THA', 'VNM', 'KHM', 'CHN')
SEasia <- admin0_shp[admin0_shp$COUNTRY_ID %in% countries,]
SEasia2 <- admin0_shp[admin0_shp$COUNTRY_ID %in% countries2,]


#extract Admin1 codes
poly_codes1.0 <-admin1_shp$GAUL_CODE[admin1_shp$NAME%in%poly_data1$Site_name]

#did we get them all?
data_sites1 <- unique(poly_data1$Site_name) #every site we have poly1 data on
site_names1 <- admin1_shp$NAME[admin1_shp$GAUL_CODE%in%poly_codes1.0] #every site extracted successfuly 

#print names not shared
missing_pts1 <- setdiff(data_sites1,site_names1)
missing_pts1 

#find coords of excluded points. 
missing_c1 <- t(data.frame(sapply(missing_pts1, function(x){ 
  z <- c(unique(poly_data1$Longitude[poly_data1$Site_name==x]), 
         unique(poly_data1$Latitude[poly_data1$Site_name==x]) )})))

# unique(poly_data1$Host[poly_data1Site_name%in%missing_pts]) returns "human"
# so when dividing by species later, just add these codes into the human set and leave others. 

#create SpatialPoints dataframe, extract corresponding polygons
unknown_pts1 <- SpatialPoints(missing_c1, proj4string = CRS(st_crs(SEasia)$input))
extract(admin1_shp, unknown_pts1)

#add these into poly_codes set
poly_codes1.1 <- c(poly_codes1.0, unique(extract(admin1_shp, unknown_pts1)$GAUL_CODE) )

#create shapefiles for these polygons:
admin1 <- get(paste0('admin', 1, '_shp'))
poly_shapes1 <- admin1[admin1$GAUL_CODE %in% poly_codes1.1,]
shapefile(poly_shapes1, paste0('~MTully/Pk_data/poly_shps1.shp'), overwrite=TRUE)
admin1_polys <- shapefile('~MTully/Pk_data/poly_shps1.shp')

#level 3 need to be added to level 2  before repeating for admin2
missing_lv3<- t(data.frame(sapply(poly_data3$Site_name, function(x){ 
  z <- c(unique(poly_data3$Longitude[poly_data3$Site_name==x]), 
         unique(poly_data3$Latitude[poly_data3$Site_name==x]) )})))
#human
missing_hlv3<- t(data.frame(sapply(hpoly_data3$Site_name, function(x){ 
  z <- c(unique(poly_data3$Longitude[poly_data3$Site_name==x]), 
         unique(poly_data3$Latitude[poly_data3$Site_name==x]) )})))
#monkey (no mozzie)
missing_mklv3<- t(data.frame(sapply(mkpoly_data3$Site_name, function(x){ 
  z <- c(unique(poly_data3$Longitude[poly_data3$Site_name==x]), 
         unique(poly_data3$Latitude[poly_data3$Site_name==x]) )})))

unknown_pts_lv3 <- SpatialPoints(missing_lv3, proj4string = CRS(st_crs(SEasia)$input))
extract(admin2_shp, unknown_pts_lv3)
unique(poly_data3$Host) #prints Human & Macaque, redo by species:

#human
unknown_pts_hlv3 <- SpatialPoints(missing_hlv3, proj4string = CRS(st_crs(SEasia)$input))
extract(admin2_shp, unknown_pts_hlv3)
#monkey
unknown_pts_mklv3 <- SpatialPoints(missing_mklv3, proj4string = CRS(st_crs(SEasia)$input))
extract(admin2_shp, unknown_pts_mklv3)


#extract GAUL codes for data set site names at Admin2:
poly_codes2.0 <-admin2_shp$GAUL_CODE[admin2_shp$NAME%in%poly_data2$Site_name]

#did we get them all?
data_sites2 <- unique(poly_data2$Site_name)
site_names2 <- admin2_shp$NAME[admin2_shp$GAUL_CODE%in%poly_codes2.0]  

#print names not shared
missing_pts2 <- setdiff(data_sites2,site_names2)

#find coords of excluded points. 
missing_c2 <- t(data.frame(sapply(missing_pts2, function(x){ 
  z <- c(unique(poly_data2$Longitude[poly_data2$Site_name==x]), 
         unique(poly_data2$Latitude[poly_data2$Site_name==x]) )})))

# unique(poly_data2$Host[poly_data2$Site_name%in%missing_pts]) returns "human"
# so when doing by species, just add these codes into the human set and leave others. 

#create SpatialPoints dataframe, extract corresponding polygons
unknown_pts2 <- SpatialPoints(missing_c2, proj4string = CRS(st_crs(SEasia)$input))
extract(admin2_shp, unknown_pts2)

#add these into poly_codes set, and add lv 3
poly_codes2.1 <- c(poly_codes2.0, unique(extract(admin2_shp, unknown_pts2)$GAUL_CODE), 
                      unique(extract(admin2_shp, unknown_pts_lv3)$GAUL_CODE) )

#create shapefiles for these polygons:
admin2 <- get(paste0('admin', 2, '_shp'))
poly_shapes2 <- admin2[admin2$GAUL_CODE %in% poly_codes2.1,]
shapefile(poly_shapes2, paste0('poly_shps2.shp'), overwrite=TRUE)
admin2_polys <- shapefile('poly_shps2.shp')

#custom polygons!
cnames <- unique(poly_dataC$Site_name)

c_poly1_data <- poly_dataC[poly_dataC$Site_name==cnames[1],]
c_poly2_data <- poly_dataC[poly_dataC$Site_name==cnames[2],]
c_poly3_data <- poly_dataC[poly_dataC$Site_name==cnames[3],]
c_poly1_pts <- SpatialPoints(c_poly1_data[c(23, 24)], proj4string = CRS(st_crs(SEasia)$input))
c_poly2_pts <- SpatialPoints(c_poly2_data[c(23, 24)], proj4string = CRS(st_crs(SEasia)$input))
c_poly3_pts <- SpatialPoints(c_poly3_data[c(23, 24)], proj4string = CRS(st_crs(SEasia)$input))
c_poly1 <- polygons(convHull(c_poly1_pts))
c_poly2 <- polygons(convHull(c_poly2_pts))
c_poly3 <- polygons(convHull(c_poly3_pts))

#redo by species:
# human level 1, add in the extra point
hpoly_codes1.0 <- admin1_shp$GAUL_CODE[admin1_shp$NAME%in%hpoly_data1$Site_name]
hpoly_codes1.1 <- c(hpoly_codes1.0, unique(extract(admin1_shp, unknown_pts1)$GAUL_CODE) )
hpoly_shapes1 <- admin1[admin1$GAUL_CODE %in% hpoly_codes1.1,]
shapefile(hpoly_shapes1, paste0('hpoly_shps1.shp'), overwrite=TRUE)
admin1_polys.h <- shapefile('hpoly_shps1.shp')

# monkey level 1 (mosquito: n/a), no extra pt to add in
mkpoly_codes1 <- admin1_shp$GAUL_CODE[admin1_shp$NAME%in%mkpoly_data1$Site_name]
mkpoly_shapes1 <- admin1[admin1$GAUL_CODE %in% mkpoly_codes1,]
shapefile(mkpoly_shapes1, paste0('mkpoly_shps1.shp'), overwrite=TRUE)
admin1_polys.mk <- shapefile('mkpoly_shps1.shp')

# human level 2, add in the extra points in 
hpoly_codes2.0 <- admin2_shp$GAUL_CODE[admin2_shp$NAME%in%hpoly_data2$Site_name]
hpoly_codes2.1 <- c(hpoly_codes2.0, unique(extract(admin2_shp, unknown_pts2)$GAUL_CODE), unique(extract(admin2_shp, unknown_pts_hlv3)$GAUL_CODE) )
hpoly_shapes2 <- admin2[admin2$GAUL_CODE %in% hpoly_codes2.1,]
shapefile(hpoly_shapes2, paste0('~MTully/Pk_data/hpoly_shps2.shp'), overwrite=TRUE)
admin2_polys.h <- shapefile('~MTully/Pk_data/hpoly_shps2.shp')

# monkey level 2 no need to add in extra pt
mkpoly_codes2.0 <- admin2_shp$GAUL_CODE[admin2_shp$NAME%in%mkpoly_data2$Site_name]
mkpoly_codes2.1 <- c(mkpoly_codes2.0, unique(extract(admin2_shp, unknown_pts_mklv3)$GAUL_CODE) )
mkpoly_shapes2 <- admin2[admin2$GAUL_CODE %in% mkpoly_codes2.1,]
shapefile(mkpoly_shapes2, paste0('~MTully/Pk_data/mkpoly_shps2.shp'), overwrite=TRUE)
admin2_polys.mk <- shapefile('~MTully/Pk_data/mkpoly_shps2.shp')

# mosquito level 2 no need to add in extra pt
mspoly_codes2 <- admin2_shp$GAUL_CODE[admin2_shp$NAME%in%mspoly_data2$Site_name]
mspoly_shapes2 <- admin2[admin2$GAUL_CODE %in% mspoly_codes2,]
shapefile(mspoly_shapes2, paste0('~MTully/Pk_data/mspoly_shps2.shp'), overwrite=TRUE)
admin2_polys.ms <- shapefile('~MTully/Pk_data/mspoly_shps2.shp')

pt_human <- point_data[which(point_data$Host=='human'),]
pt_mozzie <- point_data[which(point_data$Host=='mosquito'),]
pt_monkey <- point_data[which(point_data$Host=='macaque'),]

#species plot RGB
plot(SEasia, lwd=0.1, col="grey99", border="grey50", bg="grey84")
plot(admin1_polys.mk, lwd=0.05, col='#ccffda', border="#728f7b", add=TRUE)
plot(admin1_polys.h, lwd=0.05, col="#ffcccc", border="#8f6d6d", add=TRUE)
plot(admin2_polys.mk, lwd=0.05, col='#a1fc9a', border="#728f7b", add=TRUE)
plot(admin2_polys.ms, lwd=0.05, col='#9ac8fc', border="#727a8f", add=TRUE)
plot(admin2_polys.h, lwd=0.05, col="#fc9a9a", border="#8f6d6d", add=TRUE)
points(pt_human$Longitude, pt_human$Latitude, pch=21, col='#c43f2d', cex=0.6, lwd = 0.8)
points(pt_mozzie$Longitude, pt_mozzie$Latitude, pch=21, col='#214dd1', cex=0.6, lwd = 0.8)
points(pt_monkey$Longitude, pt_monkey$Latitude, pch=21, col='#4fa626', cex=0.6, lwd = 0.8)

#species plot Purple, Orange, Green
plot(SEasia2, lwd=0.1, col="grey99", border="grey50", bg="grey84")
plot(admin1_polys.mk, lwd=0.05, col='#c0d5e9', border="#657985", add=TRUE)
plot(admin1_polys.h, lwd=0.05, col="#c7e9c0", border="#658571", add=TRUE)
plot(admin2_polys.mk, lwd=0.05, col='#98bfd9', border="#657985", add=TRUE)
plot(admin2_polys.ms, lwd=0.05, col='#c19bd9', border="#9a8ba3", add=TRUE)
plot(admin2_polys.h, lwd=0.05, col="#a1d99b", border="#658571", add=TRUE)
points(pt_human$Longitude, pt_human$Latitude, pch=21, col='#1c6936', cex=0.4, lwd = 0.7)
points(pt_mozzie$Longitude, pt_mozzie$Latitude, pch=21, col='#5a238b', cex=0.4, lwd = 0.7)
points(pt_monkey$Longitude, pt_monkey$Latitude, pch=21, col='#23598b', cex=0.4, lwd = 0.7)
#needs suitable cropping to area of interest, also, Bangladesh is missing


#legend
fig <- plot(c(1, 1000, 2000), c(9, 1000, 2000), xaxt='n', yaxt='n', xlab="", ylab='', 
            col.axis='white', pch=1, col =c('white', '#1c6936', 'white'), cex=14, lwd=19)
legend(-140, 2100, bg="white", legend = c("Human:", "Admin 1 Polygon  ", "Admin 2 Polygon  ", "Point","", 
"Macaque:", "Admin 1 Polygon  ", "Admin 2 Polygon  ", "Point", "",
"Mosquito:", " ", "Admin 2 Polygon  ", "Point", ""), 
col = c('white', "#c7e9c0","#a1d99b", '#238b45', 
        'white', 'white', "#c0d5e9","#98bfd9", '#23598b',
        'white', 'white', 'white', "#c19bd9", '#5a238b', "white"), 
ncol=3,
pch = c(21, 15, 15, 1, 1, 1, 15, 15, 1, 21, 21, 21, 15, 21), cex=2.1, pt.cex=3.8, xpd=TRUE)
#This just creates a legend, it isn't superimposed over the map unfortunately
