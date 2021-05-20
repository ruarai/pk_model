# Script to generate accessibility to cities layer,
# using code/data from MAP's global accessibility surface project
# (Weiss et al. 2018)

# Code below adapted from Amelia Bertozzi-Villa's blogpost:
# https://medium.com/@abertozz/mapping-travel-times-with-malariaatlas-and-friction-surfaces-f4960f584f08

## Packages
library(gdistance)
library(abind)
library(rje)
#library(ggplot2)
library(malariaAtlas)

#setwd("/data/cephfs/punim1228/knowlesi")
outpath = 'output/IDN/'

## Plot defaults
theme_set(theme_minimal(base_size=14))

# require IDN_ras: Used in other scripts as IDN mask.
IDN_ras = raster('output/IDN/admin_IDN.tif')

# retrieve .shp of Indonesia
indo.shp <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin1")
plot(indo.shp, main="Shape for Clipping")

# can grab different admin levels:
# indo.shp4 <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin4")

# retrieve friction surface (for Indonesia)
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = indo.shp)
# visualise the friction surface with MAP's function or base graphics:
malariaAtlas::autoplot_MAPraster(friction)
plot(friction, main = "Friction surface: Land-based travel speed in 2015",
    xlab = "Long", ylab = "Lat")

# generate transition matrix. (8 = no' adjacent pixels for each pixel)
Tr <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(Tr) 

# Point locations - REQUIRE CITIES (50,000 P) LATLONGS as reference points
# (At these locations, accessibility is zero, and all other travel times are to one of these points)
# An open source dataset of city locations:
# https://simplemaps.com/data/world-cities
# (You'll need to grab and place into working dir manually if not working out of the codeshare)
cities <- read.csv("data/raw/simplemaps_worldcities_basicv1/worldcities.csv")

# keep cities with populations > 50000
point.locations <- cities[-which(cities$population < 50000), c("lng","lat","city")]
names(point.locations) <- c("X_COORD", "Y_COORD", "name")

# Keep only point coordinates within the shapefile bounds
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(indo.shp)
overlap <- over(point.locations, indo.shp)
point.locations <- point.locations[!is.na(overlap$name_0),]

# cast to matrix of latlongs
point.coords <- as.matrix(point.locations@coords)

# accumulated cost surface
access.raster <- gdistance::accCost(T.GC, point.coords)

# match extent to extent of IDN admin raster
access.raster <- projectRaster(access.raster, IDN_ras)

writeRaster(access.raster,
            file = paste0(outpath, 'accessibility_raster'),
            format = 'GTiff',
            overwrite = TRUE)

###############################################################################

#p <- malariaAtlas::autoplot_MAPraster(access.raster, 
#                                shp_df=indo.shp)

#full_plot <- p[[1]] + geom_point(data=data.frame(point.locations@coords), 
#                                 aes(x=X_COORD, y=Y_COORD)) +
#  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
#                                                   start=1.5, 
#                                                    r=-1.0,
#                                                    hue=1.5,
#                                                    n=16)),
#                        name="Minutes \n of Travel") +
#   ggtitle("Travel Time to Most Accessible Settlement") +
#   theme(axis.text=element_blank(),
#         panel.border=element_rect(fill=NA, color="white"))
# 
# png(paste0(outpath, 'accessibility_layer.png'),
#     width = 2000,
#     height = 2000,
#     pointsize = 30)
# 
# print(full_plot)

#dev.off()

