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
caribou <- caribou[EASTING %between% c(690000, 800000) &
                     NORTHING %between% c(5470000, 5520000)]

# Sub by date 
# caribou <- caribou[JDate > 90 & JDate < 365]

# Sub by animals that swam
# TODO: why explicitly selecting?
# swimmers <- caribou[ANIMAL_ID == "FO2016011" |
#                       ANIMAL_ID == "FO2017001" |
#                       ANIMAL_ID == "FO2017013"]


### Extract islands ----
# Extract points on different islands
caribou[, island := extract(r, matrix(c(EASTING, NORTHING), ncol = 2))]

# Count locs by island
caribou[, .N, island]

# Cut points that aren't on an island
# fogonum <- 32280
swimmers <- caribou[!is.na(island)]

# rename island
# TODO: update... these values dont match island numbers 
# swimmers$StartIsland[swimmers$island == 43] <- "Fogo"
# swimmers$StartIsland[swimmers$island == 53] <- "North Long"
# swimmers$StartIsland[swimmers$island == 55] <- "North Long"
# swimmers$StartIsland[swimmers$island == 58] <- "Blundon"
# swimmers$StartIsland[swimmers$island == 67] <- "Brother"
# swimmers$StartIsland[swimmers$island == 68] <- "W. Indian"
# swimmers$StartIsland[swimmers$island == 70] <- "South Long"
# swimmers$StartIsland[swimmers$island == 71] <- "E. Indian"
# swimmers$StartIsland[swimmers$island == 74] <- "Kate"


# Determine between which island swimming occured
swimmers[, endisland := data.table::shift(island, type = "lead")]
swimmers[island != endisland, 
         diff := paste(island, endisland, sep = '-'), 
         by = .(ANIMAL_ID, Year)]

# Count number of fixes on each island
swimmers[, counter := .N, island]
# TODO: get duration on islands rowid(rleid(StartIsland))]

# Routes
# TODO: careful island != isnt because endisland is na
swimmers[, i := .I]
swimmers[island != endisland & !is.na(endisland), swim := 'start']
swimmers[i %in% swimmers[!is.na(swim), i+1], swim := 'end']

swimmers[swim == 'start', event := .I]
swimmers[order(i)][swim == 'end', event := data.table::shift(event, 1, 'lag')]

ggplot(swimmers[!is.na(swim)]) +
  geom_line(aes(EASTING, NORTHING, color = ANIMAL_ID, )) + 
  

duration <- swimmers[island != endisland]


### Output ----
fwrite(duration, "output/duration.csv")


### Maps ----
mapview(
  swimmers[!is.na(swim)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm21N
)
mapview(
  duration,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm21N
)

mapview(
  caribou[island != 32280],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm21N
)


