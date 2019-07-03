library(data.table)
library(rgdal)
library(ggplot2)
library(maptools)
library(osmdata)
library(dplyr)


caribou <- fread('input/FogoCaribou.csv')
## Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]


# Sub by bounding box
caribou <- caribou[EASTING > 690000 & EASTING < 800000 &
                             NORTHING > 5470000  & NORTHING < 5520000]

## load Fogo shapefile
Fogo <- readShapeSpatial("/Users/quinnwebber/Dropbox/Fogo/RSF_for_app/Code and files/FogoPoly.shp")

caribou <- caribou[JDate > 90 & JDate < 330]

##
FO2016011 <- caribou[ANIMAL_ID == "FO2016011"]
FO2017001 <- caribou[ANIMAL_ID == "FO2017001"]
FO2017013 <- caribou[ANIMAL_ID == "FO2017013"]


ggplot(Fogo) + 
  geom_polygon(aes(long,lat,group=group)) +
  #ylim(5490000, 5500000) +
  #ylim(5490000, 5494000) +
  #xlim(690000,705000) +
  geom_point(data = FO2016011, aes(EASTING, NORTHING, color = ANIMAL_ID),
             alpha =0.5) +
  geom_path(data = FO2016011, aes(EASTING, NORTHING, color = ANIMAL_ID),
            color = "black", alpha = 0.1)

