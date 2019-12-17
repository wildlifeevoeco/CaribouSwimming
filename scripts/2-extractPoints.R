

### Packages ----
libs <- c('data.table', 'ggplot2', 'dplyr',
          'raster', 'prettymapr', 'rosm', 
          'stars', 'fasterize', 'rgdal')
lapply(libs, require, character.only = TRUE)

## load data
caribou <- fread('input/FogoCaribou.csv')
r <- readRDS('output/islandRaster.RDS')

## Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

## Sub by bounding box
caribou <- caribou[EASTING > 690000 & EASTING < 800000 &
                     NORTHING > 5470000  & NORTHING < 5520000]

## Sub by date 
caribou <- caribou[JDate > 90 & JDate < 330]

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
swimmers$islands2[swimmers$islands == 43] <- "Fogo"
swimmers$islands2[swimmers$islands == 53] <- "North Long"
swimmers$islands2[swimmers$islands == 55] <- "North Long"
swimmers$islands2[swimmers$islands == 58] <- "Blundon"
swimmers$islands2[swimmers$islands == 67] <- "Brother"
swimmers$islands2[swimmers$islands == 68] <- "W. Indian"
swimmers$islands2[swimmers$islands == 70] <- "South Long"
swimmers$islands2[swimmers$islands == 71] <- "E. Indian"
swimmers$islands2[swimmers$islands == 74] <- "Kate"

## determine when swimming occurred 
swimmers[, difference := data.table::shift(islands, type = "lead") - islands, by = .(ANIMAL_ID, Year)]

## rename the shifted difference column to the inhabited island
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
swimmers$islands3[swimmers$difference == 0] <- "Stay"


swimmers[, counter := rowid(rleid(islands3))]



ggplot(swimmers[counter == 1 & ANIMAL_ID == "FO2016011"], aes(EASTING, NORTHING)) +
  geom_path() +
  geom_point() +
  facet_wrap(~Year)

swimmers[counter == 1]

swimmers2 <- swimmers[difference !=0]
swimmers2$swimDir <- paste(swimmers2$islands2, swimmers2$islands3, sep = "_")

### summary stats
## calculate area

areaIsl <- data.table(st_area(coordsOSM3))
areaIsl$island <- rownames(areaIsl)
areaIsl$island[areaIsl$island == 43] <- "Fogo"
areaIsl$island[areaIsl$island == 53] <- "North Long"
areaIsl$island[areaIsl$island == 55] <- "North Long"
areaIsl$island[areaIsl$island == 58] <- "Blundon"
areaIsl$island[areaIsl$island == 67] <- "Brother"
areaIsl$island[areaIsl$island == 68] <- "W. Indian"
areaIsl$island[areaIsl$island == 70] <- "South Long"
areaIsl$island[areaIsl$island == 71] <- "E. Indian"
areaIsl$island[areaIsl$island == 74] <- "Kate"
areaIsl$V1 <- areaIsl$V1/100000



swimmers2[, .N, by = c("ANIMAL_ID", "swimDir")]

