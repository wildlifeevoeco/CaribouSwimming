### Extract Island Caribou ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'ggplot2',
  'igraph',
  'raster',
  'mapview',
  'rgdal',
  'ggnetwork'
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
swimmers <- copy(caribou)#[!is.na(island)]

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


# Count NAs
swimmers[, numbNA := sum(is.na(island)), ANIMAL_ID]

# Fill NAs with values from above
swimmers[, island := tidyr::fill(data = .SD, island)[c('island')]]

# Determine between which islands swimming occured
swimmers[, endisland := data.table::shift(island, type = "lead")]

# Set order to idate, itime
setorder(swimmers, idate, itime)

# Relocation id by individual 
swimmers[, i := seq.int(.N), ANIMAL_ID]

# Set first row for each individual to "start" island and endisland island
swimmers[i == 1, endisland := island]
swimmers[i == 1, island := 99999]

# Directed edges
swimmers[island != endisland, 
         diff := paste(island, endisland, sep = '-'), 
         by = .(ANIMAL_ID, Year)]


# Island run by individiual
swimmers[, islandrun := rleid(island), ANIMAL_ID]


# Count number of fixes on each island
swimmers[, islandCountTotal := .N, island]
swimmers[, islandCountID := .N, .(ANIMAL_ID, island)]


# Edges 
edges <- swimmers[island != endisland]
edges[, c('endislanddate', 'endislanditime', 'endislandEAST', 'endislandNORTH') := 
        data.table::shift(.SD, 1),
      .SDcols = c('idate', 'itime', 'EASTING', 'NORTHING'),
      by = ANIMAL_ID]
edges[, edgeID := .I]
# TODO : what are northern ones

edges <- edges[island != 99999 & 
                 NORTHING < 5497000 & 
                 endislandNORTH < 5497000 & 
                 ANIMAL_ID != 'FO2016001']


net <- graph_from_data_frame(
  edges[, .N, .(island, endisland)], directed = TRUE,
  vertices = edges[, .(xisl = mean(EASTING), yisl = mean(NORTHING),
                       xendisl = mean(endislandEAST), yendisl = mean(endislandNORTH)), island]
)

ggplot(net, aes(xisl, yisl, xend = xendisl, yend = yendisl)) +
  geom_edges(aes(size = N)) + geom_nodes() + geom_nodetext(aes(label = name))

(gnn <- (rasterVis::gplot(r) + geom_tile(aes(fill = value))) +
  #   ggplot(
  # edges,
  # aes(
  #   x = EASTING,
  #   y = NORTHING)
#) +
    # ylim(min(edges$NORTHING) + 1000, max(edges$NORTHING) + 1000) +
    # coord_equal() + 
    geom_edges(data = edges, aes(x = EASTING,
                   y = NORTHING, 
                   xend = endislandEAST,
                   yend = endislandNORTH,
                   color = diff)
    ) +
    geom_nodes(data = edges, aes(x = EASTING,
                                 y = NORTHING))#aes(color = vertex.names), size = 5) #+
    + guides(color = FALSE, fill = FALSE)
    # scale_color_viridis_d() + 
    # guides(color = FALSE, size = FALSE) +
    # geom_text(aes(x, y, xend = NULL, yend = NULL, label = label), data = labels) + 
    # p
)

View(swimmers[ANIMAL_ID == 'FO2016011'])

# TODO: careful island != isnt because endisland is na


### Maps ----
mapview(
  edges,
  xcol = 'endislandEAST',
  ycol = 'endislandNORTH',
  zcol = 'endisland',
  crs = utm21N
)

mapview(
  edges,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'island',
  crs = utm21N
)

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

