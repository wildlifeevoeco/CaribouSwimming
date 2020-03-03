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


### Generate igraph network ----
net <- graph_from_data_frame(
  edges[, .N, .(island, endisland)], directed = TRUE,
  vertices = edges[, .(xisl = mean(EASTING), yisl = mean(NORTHING),
                       xendisl = mean(endislandEAST), yendisl = mean(endislandNORTH)), 
                   island]
)

ggplot() + geom_sf(data = islands, fill = 'beige', alpha = 0.45) +  
  # scale_fill_manual(values = c('#d7efee', '#afa89a'), limits = c('0', '1')) +
  geom_edges(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl, size = N)) + 
  ylim(min(edges$NORTHING) - 1000, max(edges$NORTHING) + 1000) +
  xlim(min(edges$EASTING) - 1000, max(edges$EASTING) + 1000) +
  coord_sf() #+
# geom_nodes(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl))# + 
# geom_nodetext(data = net, aes(xisl, yisl, label = name))

pdf('graphics/each-id-edges-fullyear.pdf')
lapply(edges[, unique(ANIMAL_ID)], function(id) {
  (gnn <- (ggplot(data = edges[ANIMAL_ID == id]) + 
             geom_sf(data = islands, aes(fill = id))) +  
     ylim(edges[ANIMAL_ID == id, min(NORTHING) - 1000], edges[ANIMAL_ID == id, max(NORTHING) + 1000]) +
     xlim(edges[ANIMAL_ID == id, min(EASTING) - 1000], edges[ANIMAL_ID == id, max(EASTING) + 1000]) +
     # ylim(min(edges$NORTHING) - 1000, max(edges$NORTHING) + 1000) +
     # xlim(min(edges$EASTING) - 1000, max(edges$EASTING) + 1000) +
     geom_edges(aes(x = EASTING,
                    y = NORTHING, 
                    xend = endislandEAST,
                    yend = endislandNORTH,
                    color = ANIMAL_ID)
     ) +
     geom_nodes(aes(x = EASTING,
                    y = NORTHING, shape = icefree))#aes(color = vertex.names), size = 5) #+
   +  geom_nodes(aes(x = endislandEAST,
                     y = endislandNORTH, shape = icefree))#aes(color = vertex.names), size = 5) #+
   + guides(color = FALSE, fill = FALSE) + 
     facet_wrap (~ANIMAL_ID) +
     scale_shape_manual(values = c('TRUE' = 2, 'FALSE' = 4))
   # scale_color_viridis_d() + 
   # guides(color = FALSE, size = FALSE) +
   # geom_text(aes(x, y, xend = NULL, yend = NULL, label = label), data = labels) + 
   # p
  )
})
dev.off()
View(caribou[ANIMAL_ID == 'FO2016011'])

# TODO: careful island != isnt because endisland is na


### Maps ----
mapview(
  caribou[ANIMAL_ID %in% edges$ANIMAL_ID],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'island',
  crs = utm
)


mapview(
  edges,
  xcol = 'endislandEAST',
  ycol = 'endislandNORTH',
  zcol = 'endisland',
  crs = utm
)

mapview(
  edges,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'island',
  crs = utm
)

mapview(
  caribou[!is.na(island)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

mapview(
  duration,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

mapview(
  caribou[island != 32280],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)


mapview(
  caribou[is.na(island)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

