### Generate edges and island network ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'igraph'
)

lapply(libs, require, character.only = TRUE)


### Data ----
caribou <- readRDS('output/islands-locs.Rds')


### Subset ----
# Only between two different islands
edges <- caribou[island != endisland]

# Edge ID
edges[, edgeID := .I]


edges <- edges[!(between(i, 946, 947, incbounds = TRUE) & ANIMAL_ID == 'FO2017012') &
                 !(between(i, 3771, 3772, incbounds = TRUE) & ANIMAL_ID == 'FO2017013')]
# NORTHING < 5497000 &
# endislandNORTH < 5497000 &
#ANIMAL_ID != 'FO2016001']


### Variables ----
# Step length
difXY <- c('difX', 'difY')
edges[, (difXY) := .((.SD[[1]] - .SD[[3]]) ^ 2, (.SD[[2]] - .SD[[4]]) ^ 2),
      .SDcols = c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')]
edges[, stepLength := sqrt(rowSums(.SD, na.rm = TRUE)),
      .SDcols = difXY]


### Counts ----
edges[, .N, ANIMAL_ID]
edges[, .N, .(island, endisland)]

### Generate igraph network ----
net <- graph_from_data_frame(
  edges[, .N, .(island, endisland)], directed = TRUE,
  vertices = edges[, .(xisl = mean(EASTING), yisl = mean(NORTHING),
                       xendisl = mean(endislandEAST), yendisl = mean(endislandNORTH)), 
                   island]
)


saveRDS(edges, 'output/island-edges.Rds')
saveRDS(net, 'output/island-network.Rds')
