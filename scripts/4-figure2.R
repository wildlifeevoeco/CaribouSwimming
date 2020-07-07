### Figure 2 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'dplyr',
  'sf',
  'ggsflabel',
  'ggnetwork',
  'patchwork'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS('output/islandsPoly.Rds')
edges <- readRDS('output/island-edges.Rds')
net <- readRDS('output/island-network.Rds')

## summary stats for % swims per island
edges[, .N, by = island] 

# CRS
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')


### Drop some edges ----
edges <- edges[!i %in% c(59, 11156, 4254, 4859, 11157)]

### Figure 1 ----
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

themeHist <- theme(panel.border = element_rect(size = 1, fill = NA),
                   panel.background = element_rect(fill = 'white'), 
                   axis.text = element_text(size = 11, color = 'black'),
                   axis.title = element_text(size = 12, color = 'black'))

# First 
tofirst <- c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')
outfirst <- c(x = 'firstX', y = 'firstY', xend = 'endfirstX', yend = 'endfirstY')

edges[, (outfirst) := .SD,
      by = .(island, ANIMAL_ID), .SDcols = tofirst]


# North/South regions
edges[, region := ifelse(firstY < median(firstY) + 6500, 'South', 'North')]
N <- edges[season == 'icefree' & region == 'North']
S <- edges[season == 'icefree' & region == 'South']

# Bboxes
Nbox <- c(ymin = min(N$NORTHING), 
          ymax = max(N$NORTHING),
          xmin = min(N$EASTING), 
          xmax = max(N$EASTING))
Nsfbox <- st_as_sf(st_as_sfc(st_bbox(Nbox, crs = utm)))
Nsfbox$label <- 'C'

Sbox <- c(ymin = min(S$NORTHING) - 1510, 
          ymax = max(S$NORTHING) + 1510,
          xmin = min(S$EASTING) - 1000, 
          xmax = max(S$EASTING))
Ssfbox <- st_as_sf(st_as_sfc(st_bbox(Sbox, crs = utm)))
Ssfbox$label <- 'D'


# Palette
pal <- unique(edges, by = 'ANIMAL_ID')[order(region), .(ID = unique(ANIMAL_ID), col = scales::viridis_pal()(.N))]
cols <- pal[, setNames(col, ID)]

# Base islands
labels <- data.table(id = c(120, 124, 128),
                     label = c('Fogo Island', 'E. Perry Island', 'W. Perry Island'))
islands <- left_join(islands, labels, 'id')

(gfogo <- ggplot(islands) + 
    geom_sf(fill = islandcol, size = 0.13, color = coastcol) + 
    themeMap +
    theme(axis.text = element_text(size = 11, color = 'black')) +
    scale_y_continuous(label = function(x) sprintf('%.2f°N', x)) +
    scale_x_continuous(label = function(x) sprintf('%.1f°W', -1 * x)))

(gfogothick <- ggplot(islands) + 
    geom_sf(fill = islandcol, size = 0.3, color = coastcol) + 
    themeMap +
    theme(axis.text = element_text(size = 11, color = 'black')))


# Histogram
(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID),
                   binwidth = 10) +
    guides(fill = FALSE) +
    scale_fill_manual(values = cols) +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365)) + 
    labs(x = 'Calendar Day', y = NULL) + 
    themeHist)

# Edges
edgesize <- 1
(gnetN <- gfogothick +
    geom_edges(data = N,
               aes(
                 x = firstX,
                 y = firstY,
                 xend = endfirstX,
                 yend = endfirstY,
                 color = ANIMAL_ID
               ),
               size = edgesize, alpha = 0.8
    ) +
    ylim(Nbox[['ymin']], Nbox[['ymax']]) +
    xlim(Nbox[['xmin']], Nbox[['xmax']]) +
  guides(color = FALSE) +
  scale_color_manual(values = cols) +
  labs(x = NULL, y = NULL) + 
  themeMap + 
  theme(axis.ticks = element_blank())
  )

(gnetS <- gfogothick +
    geom_edges(data = S,
               aes(
                 x = firstX,
                 y = firstY,
                 xend = endfirstX,
                 yend = endfirstY,
                 color = ANIMAL_ID
               ),
               size = edgesize, alpha = 0.8
    ) +
    geom_point(aes(east, north), size = 3, fill = '#8a8fd4', shape = 23,
               data = data.table(east = 699057.51, 
                                 north = 5491600.29)) + 
    ylim(Sbox[['ymin']], Sbox[['ymax']]) +
    xlim(Sbox[['xmin']], Sbox[['xmax']]) +
  guides(color = FALSE) +
  scale_color_manual(values = cols) +
  labs(x = NULL, y = NULL) + 
  themeMap + 
  theme(axis.ticks = element_blank())
)

(withboxes <- gfogo +
    geom_sf(
      data = Nsfbox,
      fill = NA,
      color = 'black',
      size = 0.3
    ) +
    geom_sf_label(data = Nsfbox, aes(label = label)) +
    geom_sf(
      data = Ssfbox,
      fill = NA,
      color = 'black',
      size = 0.3
    ) +
    geom_sf_label(data = Ssfbox, aes(label = label)) +
    geom_sf_label_repel(aes(label = label), 
                        data = islands[islands$label != 'Fogo Island', ], 
                        nudge_x = 1.3e4, size = 2) +
    geom_sf_label(aes(label = label), 
                        data = islands[islands$label == 'Fogo Island', ], size = 2) +
    theme(axis.title = element_blank())
)
    
 
layout <- 'AAACCCCDDDD
           AAACCCCDDDD
           BBBCCCCDDDD'

(g <- withboxes + ghist + gnetN + gnetS + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'A')
)


### Output fig ----
ggsave(
  'graphics/Fig2.png',
  width = 28,
  height = 12.5,
  units = 'cm',
  dpi = 320
)
