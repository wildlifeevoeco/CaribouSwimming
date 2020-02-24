### Extract Island Caribou ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'ggplot2',
  'igraph',
  'raster',
  'mapview',
  'rgdal'
)
lapply(libs, require, character.only = TRUE)


### Load data ----
caribou <- fread('input/FogoCaribou.csv')
r <- raster('output/fogo-land-caribou-swimming.tif')
# r <- readRDS('output/islandsRaster.Rds')

### Prep data ----
# Generate connected components
conn <- clump(r)

# Reduce small island/pixel noise
rna <- r
rna[rna != 1] <- NA
foc <- focal(rna, focalWeight(rna, 10, 'circle'), modal)
fuzz <- clump(foc)


# Datetime
caribou[, c('idate', 'itime') := .(as.IDate(idate), as.ITime(itime))]

# Project coordinates
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

# Sub by bounding box
# caribou <- caribou[EASTING %between% c(690000, 800000) &
#                      NORTHING %between% c(5470000, 5520000)]

# Sub by date 
# caribou <- caribou[JDate > 90 & JDate < 365]

# Sub by animals that swam
# TODO: why explicitly selecting?
# swimmers <- caribou[ANIMAL_ID == "FO2016011" |
#                       ANIMAL_ID == "FO2017001" |
#                       ANIMAL_ID == "FO2017013"]


### Extract islands ----
# Extract points on different islands
caribou[, island := extract(conn, matrix(c(EASTING, NORTHING), ncol = 2))]
caribou[, islfuzz := extract(fuzz, matrix(c(EASTING, NORTHING), ncol = 2))]

# Count locs by island
caribou[, .N, island]

# Cut points that aren't on an island
# fogonum <- 32280
# TODO:
swimmers <- caribou#[!is.na(island)]

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



# Determine between which islands swimming occured
swimmers[, endisland := data.table::shift(island, type = "lead")]
swimmers[island != endisland, 
         diff := paste(island, endisland, sep = '-'), 
         by = .(ANIMAL_ID, Year)]


setorder(swimmers, idate, itime)
# Relocation id by individual 
swimmers[, i := seq.int(.N), ANIMAL_ID]

# Island run by individiual
swimmers[, islandrun := rleid(island), ANIMAL_ID]

swimmers[, c('island', 'endisland') := tidyr::fill(data = .SD, island, endisland)[c('island', 'endisland')]]

View(swimmers)
# Count number of fixes on each island
swimmers[, counter := .N, island]
# TODO: get duration on islands by individual rowid(rleid(StartIsland))]



View(swimmers)

# TODO: use fill rows function to spread from one island to the next
#       switch is the movement step, next island is the edge
#       and number of locs between movement steps is the duration
tidyr::fill




# Routes
# TODO: careful island != isnt because endisland is na

swimmers[island != endisland & !is.na(endisland), 
         c('swimi', 'swim') := .(i, 'start')]
swimmers[, .(swimi, data.table::shift(swimi))]
# swimmers[i %in% swimmers[!is.na(swimi), i+1], 
#          c('swimi', 'swim') := .(i, 'start')]]

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


mapview(
  caribou[is.na(island)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm21N
)

