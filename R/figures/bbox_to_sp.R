
library(sp)

bbox_to_SpatialPolygons <- function(x,proj4string=CRS(as.character(NA)))
{	
  if(class(x)[1]=="Extent")
  {
    bbox <- matrix(c(
      x@xmin,
      x@ymin,
      x@xmax,
      x@ymax
    ),nrow=2,ncol=2)
  }
  
  coords <- rbind(
    c(bbox[1,1],bbox[2,1]),
    c(bbox[1,2],bbox[2,1]),
    c(bbox[1,2],bbox[2,2]),
    c(bbox[1,1],bbox[2,2]),
    c(bbox[1,1],bbox[2,1])
  )
  
  bboxPolygon <- Polygon(coords)
  bboxPolygons <- Polygons(list(bboxPolygon),ID=1)
  bboxSpatialPolygons <- SpatialPolygons(list(bboxPolygons),proj4string=proj4string)
  return(bboxSpatialPolygons)
}

