### Extract Island Caribou ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c('data.table', 'ggplot2', 'dplyr',
          'raster', 'prettymapr', 'rosm', 
          'stars', 'fasterize', 'rgdal')
lapply(libs, require, character.only = TRUE)

## load data
caribou <- fread('input/FogoCaribou.csv')
r <- readRDS('output/islandsRaster.RDS')

## Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

## Sub by bounding box
caribou <- caribou[EASTING > 690000 & EASTING < 800000 &
                     NORTHING > 5470000  & NORTHING < 5520000]

## Sub by date 
caribou <- caribou[JDate > 90 & JDate < 365]

## Sub by animals that swam
swimmers <- caribou[ANIMAL_ID == "FO2016011" |
                      ANIMAL_ID == "FO2017001" |
                      ANIMAL_ID == "FO2017013"]

## Extract points on different islands
swimmers[, islands := extract(r, matrix(c(EASTING, NORTHING), ncol = 2))]

## check points in reference to raster map
swimmers[, .N, by = "islands"]
plot(r, xlim = c(685000, 710000), 
     ylim = c(5490000, 5502000))
par(new = T)
plot(NORTHING ~ EASTING, data = swimmers[islands == 55], 
     xlim = c(685000, 710000), 
     ylim = c(5490000, 5502000))


## cut points that aren't on an island
swimmers <- swimmers[!is.na(islands),]

## rename islands
swimmers$StartIsland[swimmers$islands == 43] <- "Fogo"
swimmers$StartIsland[swimmers$islands == 53] <- "North Long"
swimmers$StartIsland[swimmers$islands == 55] <- "North Long"
swimmers$StartIsland[swimmers$islands == 58] <- "Blundon"
swimmers$StartIsland[swimmers$islands == 67] <- "Brother"
swimmers$StartIsland[swimmers$islands == 68] <- "W. Indian"
swimmers$StartIsland[swimmers$islands == 70] <- "South Long"
swimmers$StartIsland[swimmers$islands == 71] <- "E. Indian"
swimmers$StartIsland[swimmers$islands == 74] <- "Kate"

## determine when swimming occurred 
swimmers[, difference := data.table::shift(islands, type = "lead") - islands, by = .(ANIMAL_ID, Year)]

## rename the shifted difference column to the inhabited island
swimmers$MoveIsland[swimmers$difference == -10] <- "Blundon"
swimmers$MoveIsland[swimmers$difference == 3] <- "E. Indian"
swimmers$MoveIsland[swimmers$difference == -3] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == -1] <- "Brother"
swimmers$MoveIsland[swimmers$difference == 1] <- "W. Indian"
swimmers$MoveIsland[swimmers$StartIsland == "Blundon" & 
                    swimmers$difference == 13] <- "E. Indian"
swimmers$MoveIsland[swimmers$StartIsland == "North Long" & 
                    swimmers$difference == 13] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == -13] <- "North Long"
swimmers$MoveIsland[swimmers$difference == -25] <- "Fogo"
swimmers$MoveIsland[swimmers$difference == 25] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == -15] <- "North Long"
swimmers$MoveIsland[swimmers$difference == 15] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == -6] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == 6] <- "Kate"
swimmers$MoveIsland[swimmers$difference == 2] <- "South Long"
swimmers$MoveIsland[swimmers$difference == -2] <- "W. Indian"
swimmers$MoveIsland[swimmers$difference == 0] <- "Stay"

## count number of fixes on each island
swimmers[, counter := rowid(rleid(StartIsland))]

duration <- swimmers[difference != 0]
duration[!is.na(MoveIsland)]


fwrite(duration, "output/duration.csv")
