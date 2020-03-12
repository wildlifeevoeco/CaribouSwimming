### Figure 1 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'sf',
  'igraph',
  'ggnetwork',
  'mapview'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS("output/islandsPoly.Rds")
edges <- readRDS('output/island-edges.Rds')
net <- readRDS('output/island-network.Rds')


### Figure 1 ----
# All ids
ggplot() + geom_sf(data = islands, fill = '#c7c0bd') +  
  # scale_fill_manual(values = c('#d7efee', '#afa89a'), limits = c('0', '1')) +
  geom_edges(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl, 
                             alpha = N)) + 
  ylim(min(edges$NORTHING) - 1000, max(edges$NORTHING) + 1000) +
  xlim(min(edges$EASTING) - 1000, max(edges$EASTING) + 1000) +
  coord_sf() +
  theme(panel.background = element_rect(fill = '#d0dee5'))
# geom_nodes(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl))# + 
# geom_nodetext(data = net, aes(xisl, yisl, label = name))

# By id
(gnn <- (ggplot(data = edges) +
            geom_sf(data = islands, fill = '#c7c0bd')) +
  ylim(edges[, min(NORTHING) - 1000], edges[, max(NORTHING) + 1000]) +
  xlim(edges[, min(EASTING) - 1000], edges[, max(EASTING) + 1000]) +
  geom_edges(aes(x = EASTING,
                 y = NORTHING,
                 xend = endislandEAST,
                 yend = endislandNORTH,
                 color = ANIMAL_ID)) +
  # guides(color = FALSE, fill = FALSE) +
  scale_shape_manual(values = c('TRUE' = 2, 'FALSE' = 4)) +
  # scale_color_viridis_d() +
    facet_wrap(~season) + 
  theme(panel.background = element_rect(fill = '#d0dee5'))
# guides(color = FALSE, size = FALSE) +
# geom_text(aes(x, y, xend = NULL, yend = NULL, label = label), data = labels) +
# p
)


