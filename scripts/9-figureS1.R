
### Packages ----
libs <- c(
  'data.table',
  'dplyr',
  'sf',
  'ggsflabel',
  'ggnetwork',
  'patchwork',
  'effects',
  'mapview'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS('output/islandsPoly.Rds')
edges <- readRDS('output/island-edges.Rds')

mapview(islands)

# Theme -------------------------------------------------------------------
# Colors
watercol <- '#c3e2ec'
islandcol <- '#d0c2a9'
coastcol <- '#82796a'
gridcol <- '#323232'

# Themes 
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = watercol), 
                  panel.grid = element_line(color = gridcol, size = 0.2),
                  axis.text = element_blank(),
                  axis.title = element_blank())

# North/South regions
edges[, region := ifelse(firstY < median(firstY) + 6500, 'South', 'North')]
S <- edges[season == 'icefree' & region == 'South']

# Bboxes
Sbox <- c(ymin = min(S$NORTHING) - 1510, 
          ymax = max(S$NORTHING) + 1510,
          xmin = min(S$EASTING) - 1500, 
          xmax = max(S$EASTING) + 2000)
Ssfbox <- st_as_sf(st_as_sfc(st_bbox(Sbox, crs = utm)))

# Palette
pal <- unique(edges, by = 'ANIMAL_ID')[order(region), .(ID = unique(ANIMAL_ID), col = scales::viridis_pal()(.N))]
cols <- pal[, setNames(col, ID)]


# Base islands ------------------------------------------------------------
labels <- data.table(id = c(58, ## Island 6
                            72, ## Island 7
                            70, ## Island 1
                            73, ## Island 2
                            75, ## Island 3
                            78, ## Island 4
                            120, ## Fogo
                            124, ## E. Perry
                            125, ## Island 5
                            128), ## W. Perry
                     label = c('Island 6', 'Island 7',
                               'Island 1', 'Island 2', 'Island 3', 'Island 4',
                               'Fogo Island', 'E. Perry Island','Island 5','W. Perry Island'))
islands <- left_join(islands, labels, 'id')


png('graphics/FigS1.png',
  width = 3000,
  height = 3000,
  units = 'px',
  res = 600)
ggplot(islands) + 
  geom_sf(fill = islandcol, size = 0.3, color = coastcol) + 
  guides(color = FALSE) +
  labs(x = NULL, y = NULL) + 
  geom_sf_label_repel(aes(label = label), 
                      data = islands[islands$label != 'Fogo Island', ], 
                      nudge_x = 2.2e3, size = 2) +
  geom_sf_label(aes(label = label), 
                data = islands[islands$label == 'Fogo Island', ], size = 2) +
  themeMap + 
  theme(axis.text = element_text(size = 11, color = 'black'),
        axis.ticks = element_blank())
dev.off()
