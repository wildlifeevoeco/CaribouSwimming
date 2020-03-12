### Figure 1 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'sf',
  'ggnetwork',
  'patchwork'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS("output/islandsPoly.Rds")
edges <- readRDS('output/island-edges.Rds')
net <- readRDS('output/island-network.Rds')


### Figure 1 ----
tomean <- c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')
outmean <- c(x = 'meanX', y = 'meanY', xend = 'endmeanX', yend = 'endmeanY')
edges[, (outmean) := lapply(.SD, mean), 
      by = .(island, ANIMAL_ID), .SDcols = tomean]

(gnet <- ggplot(data = edges[season == 'icefree']) +
    geom_sf(data = islands, fill = '#c7c0bd') +
    geom_edges(
      aes(
        x = meanX,
        y = meanY,
        xend = endmeanX,
        yend = endmeanY,
        color = ANIMAL_ID
      ),
      size = 2
    ) +
    # geom_point(aes(median(meanX), median(meanY) + 6500), size = 4) + 
    guides(color = FALSE) +
    scale_color_viridis_d() + 
    labs(x = NULL, y = NULL))

gcol <- ggplot(data = edges[, .N, by = .(JDate, ANIMAL_ID)]) + 
  geom_col(aes(JDate, N, color = ANIMAL_ID)) + 
  guides(color = FALSE) + 
  scale_color_viridis_d()

(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID)) +
    guides(fill = FALSE) +
    scale_fill_viridis_d() +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365))+ 
    labs(x = 'Julian Day', y = NULL))

gnet / ghist + 
  plot_layout(heights = c(3, 1))

### Other figs ----
# south/west
edges[, region := ifelse(meanY < median(meanY) + 6500, 'South', 'North')]

(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID)) +
    guides(fill = FALSE) +
    scale_fill_viridis_d() +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365))+ 
    labs(x = 'Julian Day', y = NULL) + 
    facet_wrap(~region))

(ghist <- ggplot(data = edges[, .N, .(ANIMAL_ID, region)]) +
    geom_bar(aes(N, ANIMAL_ID)) +
    guides(fill = FALSE) +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365))+ 
    labs(x = 'Julian Day', y = NULL) + 
    facet_wrap(~region))


# All ids
ggplot() + geom_sf(data = islands, fill = '#c7c0bd') +  
  # scale_fill_manual(values = c('#d7efee', '#afa89a'), limits = c('0', '1')) +
  geom_edges(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl)) + 
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


