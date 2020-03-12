### Extract island locs ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'sf',
  'raster',
  'rgdal'
)
lapply(libs, require, character.only = TRUE)


### Load data ----
caribou <- fread('input/FogoCaribou.csv')
islands <- readRDS('output/islandsPoly.Rds')

### Prep data ----
# Datetime
caribou[, c('idate', 'itime') := .(as.IDate(idate), as.ITime(itime))]

# Hour
caribou[, hour := hour(itime)]

# Project coordinates
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')
coords <- c('EASTING', 'NORTHING')

caribou[, (coords) := as.data.table(project(cbind(X_COORD, Y_COORD), utm$proj4string))]

# Sub by bounding box
# caribou <- caribou[EASTING %between% c(690000, 800000) &
#                      NORTHING %between% c(5470000, 5520000)]

# Sub by date 
caribou <- caribou[JDate > 90 & JDate < 365]

# Sub by animals that swam
# TODO: why explicitly selecting?
# caribou <- caribou[ANIMAL_ID == "FO2016011" |
#                       ANIMAL_ID == "FO2017001" |
#                       ANIMAL_ID == "FO2017013"]


### Extract islands ----
# Extract points on different islands
# caribou[, island := 
#   st_join(
#     st_as_sf(.SD, coords = coords, crs = utm),
#     st_buffer(islands, 25), 
#     join = st_intersects)$id, 
#   .SDcols = coords]

caribou[, island := 
          st_nearest_feature(
            st_as_sf(.SD, coords = coords, crs = utm),
            islands),
        .SDcols = coords]

# 
# Count locs by island
caribou[, .N, island]


# Set order to idate, itime
setorder(caribou, idate, itime)

# Count NAs
caribou[, numbNA := sum(is.na(island)), ANIMAL_ID]


# Determine between which islands swimming occured
caribou[, endisland := data.table::shift(island, n = 1L, type = 'lead'),
         ANIMAL_ID]


# Relocation id by individual 
caribou[, i := seq.int(.N), ANIMAL_ID]


# Directed edges
caribou[island != endisland, 
         diff := paste(island, endisland, sep = '-'), 
         by = .(ANIMAL_ID, Year)]


# Island run by individiual
caribou[, islandrun := rleid(island), ANIMAL_ID]
caribou[, islandlen := .N * 2 / 24, .(islandrun, ANIMAL_ID)]

# Count number of fixes on each island
caribou[, islandCountTotal := .N, island]
caribou[, islandCountID := .N, .(ANIMAL_ID, island)]


# Edges 
caribou[, c('endislanddate',
            'endislanditime',
            'endislandEAST',
            'endislandNORTH') :=
          data.table::shift(.SD, n = 1L, type = 'lead'),
        .SDcols = c('idate', 'itime', 'EASTING', 'NORTHING'),
        by = ANIMAL_ID]

### Output ----
saveRDS(caribou, 'output/islands-locs.Rds')

