### Extract Island Caribou ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'ggplot2',
  # 'dplyr',
  'raster',
  # 'prettymapr',
  # 'rosm',
  # 'stars',
  # 'fasterize',
  'mapview',
  'rgdal'
)
lapply(libs, require, character.only = TRUE)


### Load data ----
caribou <- fread('input/FogoCaribou.csv')
r <- readRDS('output/islandsRaster.Rds')

### Prep data ----
# Project coordinates
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

# Sub by bounding box
# caribou <- caribou[EASTING > 690000 & EASTING < 800000 &
#                      NORTHING > 5470000  & NORTHING < 5520000]

# Sub by date 
# caribou <- caribou[JDate > 90 & JDate < 365]

# Sub by animals that swam
# TODO: why explicitly selecting?
# swimmers <- caribou[ANIMAL_ID == "FO2016011" |
#                       ANIMAL_ID == "FO2017001" |
#                       ANIMAL_ID == "FO2017013"]


### Extract islands ----
# Extract points on different islands
caribou[, islands := extract(r, matrix(c(EASTING, NORTHING), ncol = 2))]

# Count locs by islands
caribou[, .N, islands]

# Cut points that aren't on an island
swimmers <- caribou[!is.na(islands)]

# rename islands
# TODO: update... these values dont match island numbers 
# swimmers$StartIsland[swimmers$islands == 43] <- "Fogo"
# swimmers$StartIsland[swimmers$islands == 53] <- "North Long"
# swimmers$StartIsland[swimmers$islands == 55] <- "North Long"
# swimmers$StartIsland[swimmers$islands == 58] <- "Blundon"
# swimmers$StartIsland[swimmers$islands == 67] <- "Brother"
# swimmers$StartIsland[swimmers$islands == 68] <- "W. Indian"
# swimmers$StartIsland[swimmers$islands == 70] <- "South Long"
# swimmers$StartIsland[swimmers$islands == 71] <- "E. Indian"
# swimmers$StartIsland[swimmers$islands == 74] <- "Kate"


# Determine between which islands swimming occured
swimmers[, difference := paste(islands, shift(islands, type = "lead"), sep = '-'), 
         by = .(ANIMAL_ID, Year)]

## count number of fixes on each island
swimmers[, counter := rowid(rleid(StartIsland))]

duration <- swimmers[difference != 0]
duration[!is.na(MoveIsland)]


fwrite(duration, "output/duration.csv")


### Maps ----
mapview(
  caribou[islands != 32280],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm21N
)


