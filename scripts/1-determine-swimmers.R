
library(sf)
library(sp)
library(data.table)
library(rgdal)
library(ggplot2)
library(maptools)
library(dplyr)
library(data.table)
library(plotKML)
library(ggmap)
library(osmdata)
library(cartography)
library(raster)
library(prettymapr)
library(rosm)
library(stars)
library(mapview)
library(fasterize)

caribou <- fread('input/FogoCaribou.csv')
## Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]


# Sub by bounding box
caribou <- caribou[EASTING > 690000 & EASTING < 800000 &
                             NORTHING > 5470000  & NORTHING < 5520000]

caribou <- caribou[JDate > 90 & JDate < 330]

##
swimmers <- caribou[ANIMAL_ID == "FO2016011" |
                     ANIMAL_ID == "FO2017001" |
                     ANIMAL_ID == "FO2017013"]

ns <- makebbox(49.0, -54.0, 50.0, -55.0)
ns <- makebbox(max(swimmers[ANIMAL_ID == "FO2016011"]$Y_COORD), 
               max(swimmers[ANIMAL_ID == "FO2016011"]$X_COORD),
               min(swimmers[ANIMAL_ID == "FO2016011"]$Y_COORD), 
               min(swimmers[ANIMAL_ID == "FO2016011"]$X_COORD))

ggplot(swimmers) +
  geom_point(aes(X_COORD, Y_COORD))


### extract islands from osm 

coords <- makebbox(49.5194, -54.1878, 
                   49.65, -54.3533)

coordsOSM <- osm.raster(coords, projection=CRS('+proj=utm +zone=21 ellps=WGS84'), crop=TRUE)
coordsOSM2 <- coordsOSM[[1]] != 170
coordsOSM2[coordsOSM2 == 0] <- NA
coordsOSM3 <- st_as_stars(coordsOSM2) %>% 
  st_as_sf(as_points = FALSE, use_integer = TRUE, merge = TRUE, na.rm = TRUE)

coordsOSM3$island <- seq_along(coordsOSM3$geometry)

r <- raster(ymn = st_bbox(coordsOSM3)$ymin,
            ymx = st_bbox(coordsOSM3)$ymax, 
            xmn = st_bbox(coordsOSM3)$xmin,
            xmx = st_bbox(coordsOSM3)$xmax, 
            crs = CRS('+proj=utm +zone=21 ellps=WGS84'))
r <- fasterize(coordsOSM3, r, field = "island")

swimmers[, islands := extract(r, matrix(c(EASTING, NORTHING), ncol = 2))]


swimmers[, .N, by = "islands"]

plot(r, xlim = c(685000, 710000), 
     ylim = c(5490000, 5502000))
par(new = T)
plot(NORTHING ~ EASTING, data = swimmers[islands == 71], 
     xlim = c(685000, 710000), 
     ylim = c(5490000, 5502000))

## rename islands
swimmers <- swimmers[!is.na(islands),]
 
swimmers$islands2[swimmers$islands == 43] <- "Fogo"
swimmers$islands2[swimmers$islands == 53] <- "North Long"
swimmers$islands2[swimmers$islands == 55] <- "North Long"
swimmers$islands2[swimmers$islands == 58] <- "Blundon"
swimmers$islands2[swimmers$islands == 67] <- "Brother"
swimmers$islands2[swimmers$islands == 68] <- "W. Indian"
swimmers$islands2[swimmers$islands == 70] <- "South Long"
swimmers$islands2[swimmers$islands == 71] <- "E. Indian"
swimmers$islands2[swimmers$islands == 74] <- "Kate"


swimmers[, difference := data.table::shift(islands, type = "lead") - islands, by = .(ANIMAL_ID, Year)]


swimmers$islands3[swimmers$difference == -10] <- "Blundon"
swimmers$islands3[swimmers$difference == 3] <- "E. Indian"
swimmers$islands3[swimmers$difference == -3] <- "W. Indian"
swimmers$islands3[swimmers$difference == -1] <- "Brother"
swimmers$islands3[swimmers$difference == 1] <- "W. Indian"
swimmers$islands3[swimmers$islands2 == "Blundon" & 
                       swimmers$difference == 13] <- "E. Indian"
swimmers$islands3[swimmers$islands2 == "North Long" & 
                       swimmers$difference == 13] <- "W. Indian"
swimmers$islands3[swimmers$difference == -13] <- "North Long"
swimmers$islands3[swimmers$difference == -25] <- "Fogo"
swimmers$islands3[swimmers$difference == 25] <- "W. Indian"
swimmers$islands3[swimmers$difference == -15] <- "North Long"
swimmers$islands3[swimmers$difference == 15] <- "W. Indian"
swimmers$islands3[swimmers$difference == -6] <- "W. Indian"
swimmers$islands3[swimmers$difference == 6] <- "Kate"
swimmers$islands3[swimmers$difference == 2] <- "South Long"
swimmers$islands3[swimmers$difference == -2] <- "W. Indian"

swimmers2 <- swimmers[difference !=0]
swimmers2$swimDir <- paste(swimmers2$islands2, swimmers2$islands3, sep = "_")

## summary stats

swimmers2[, .N, by = c("ANIMAL_ID", "swimDir")]

