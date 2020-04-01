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

# CRS
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')


### Drop erroneous edges ----
# TODO: move elsewhere?
edges <- edges[!i %in% c(59, 11156, 4254, 4859, 11157)]

### Figure 1 ----
# Themes 
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d6ebf2"), 
                  panel.grid = element_line(color = "black", size = 0.2),
                  axis.text = element_blank(),
                  axis.title = element_blank()
                  )

# axis.text = element_text(size = 12, color = "black")

themeHist <- theme(panel.border = element_rect(size = 1, fill = NA),
                   panel.background = element_rect(fill = "white"), 
                   axis.text = element_text(size = 11, color = "black"),
                   axis.title = element_text(size = 12, color = "black"))

# First 
tofirst <- c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')
outfirst <- c(x = 'firstX', y = 'firstY', xend = 'endfirstX', yend = 'endfirstY')

edges[, (outfirst) := .SD[sample(.N, size = 1)],
      by = .(island, ANIMAL_ID), .SDcols = tofirst]


# North/South regions
edges[, region := ifelse(firstY < median(firstY) + 6500, 'South', 'North')]
N <- edges[season == 'icefree' & region == 'North']
S <- edges[season == 'icefree' & region == 'South']

# Bboxes
Nbox <- c(ymin = min(N$NORTHING), 
          ymax = max(N$NORTHING),
          xmin = min(N$EASTING) - 250, 
          xmax = max(N$EASTING) + 250)
Nsfbox <- st_as_sf(st_as_sfc(st_bbox(Nbox, crs = utm)))
Nsfbox$label <- 'C'

Sbox <- c(ymin = min(S$NORTHING) - 1010, 
          ymax = max(S$NORTHING) + 1010,
          xmin = min(S$EASTING) - 1000, 
          xmax = max(S$EASTING))
Ssfbox <- st_as_sf(st_as_sfc(st_bbox(Sbox, crs = utm)))
Ssfbox$label <- 'D'


# Palette
pal <- unique(edges, by = 'ANIMAL_ID')[order(region), .(ID = unique(ANIMAL_ID), col = scales::viridis_pal()(.N))]
cols <- pal[, setNames(col, ID)]

# Base islands
(gfogo <- ggplot(islands) + 
    geom_sf(fill = '#d0c2a9') + 
    themeMap +
    theme(axis.text = element_text(size = 11, color = "black")))


# Histogram
(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID),
                   binwidth = 10) +
    guides(fill = FALSE) +
    scale_fill_manual(values = cols) +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365)) + 
    labs(x = 'Julian Day', y = NULL) + 
    themeHist)

# Edges
(gnetN <- gfogo +
    geom_edges(data = N,
               aes(
                 x = firstX,
                 y = firstY,
                 xend = endfirstX,
                 yend = endfirstY,
                 color = ANIMAL_ID
               ),
               size = 2
    ) +
    geom_point(data = N, aes(x = firstX, y = firstY, color = ANIMAL_ID),
               size = 1) +
    geom_point(data = N, aes(x = endfirstX, y = endfirstY, color = ANIMAL_ID),
               size = 1) +
    ylim(Nbox[['ymin']], Nbox[['ymax']]) +
    xlim(Nbox[['xmin']], Nbox[['xmax']]) +
  guides(color = FALSE) +
  scale_color_manual(values = cols) +
  labs(x = NULL, y = NULL) + 
  themeMap + 
  theme(axis.ticks = element_blank())
  )

# TODO: split scales N/S
(gnetS <- gfogo +
    geom_edges(data = S,
               aes(
                 x = firstX,
                 y = firstY,
                 xend = endfirstX,
                 yend = endfirstY,
                 color = ANIMAL_ID
               ),
               size = 2
    ) +
    geom_point(data = S, aes(x = firstX, y = firstY, color = ANIMAL_ID),
               size = 1) +
    geom_point(data = S, aes(x = endfirstX, y = endfirstY, color = ANIMAL_ID),
               size = 1) +
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
      size = 1.2
    ) + 
    geom_sf_label(data = Nsfbox, aes(label = label)) +
    geom_sf(
      data = Ssfbox,
      fill = NA,
      color = 'black',
      size = 1.2
    ) +
    geom_sf_label(data = Ssfbox, aes(label = label)) +
    theme(axis.title = element_blank())
)
    
 
# TODO: zoom out Fogo for NL context?
layout <- "AAACCCCDDDD
           AAACCCCDDDD
           BBBCCCCDDDD"

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
