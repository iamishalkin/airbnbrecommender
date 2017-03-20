#Overpass

library(rgdal)
library(dplyr)
#setwd()
moscow_attr = readOGR(dsn = "/students/aamenshikova/distances_to_poi", "overpass")
mattr_names = moscow_attr@data
mattr_coords = moscow_attr@coords
mattr_coords <- as.data.frame(mattr_coords)
mattr_names <- dplyr::select(mattr_names, Name) 

moscow_attr_coord <- cbind(mattr_names, mattr_coords)
colnames(moscow_attr_coord)<- c("name","lon","lat")
