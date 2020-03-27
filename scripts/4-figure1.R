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
edges <- edges[!i %in% c(59, 11156, 4254, 4859)]

### Figure 1 ----
# Themes 
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = "#d6ebf2"), #"#e3ebf9"),
                  panel.grid = element_line(color = "black", size = 0.2),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank())

# axis.text = element_text(size = 12, color = "black")

themeHist <- theme(panel.border = element_rect(size = 1, fill = NA),
                   panel.background = element_rect(fill = "white"), 
                   axis.text = element_text(size = 12, color = "black"),
                   axis.title = element_text(size = 14, color = "black"))

# Mean 
tomean <- c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')
outmean <- c(x = 'meanX', y = 'meanY', xend = 'endmeanX', yend = 'endmeanY')
# edges[, (outmean) := lapply(.SD, mean, na.rm = TRUE),
#       by = .(island, ANIMAL_ID), .SDcols = tomean]
edges[, (outmean) := .SD[sample(.N, size = 1)],
      by = .(island, ANIMAL_ID), .SDcols = tomean]


# North/South regions
edges[, region := ifelse(meanY < median(meanY) + 6500, 'South', 'North')]
N <- edges[season == 'icefree' & region == 'North']
S <- edges[season == 'icefree' & region == 'South']

# Bboxes
Nbox <- c(ymin = min(N$NORTHING), 
          ymax = max(N$NORTHING),
          xmin = min(N$EASTING) - 250, 
          xmax = max(N$EASTING) + 250)
Nsfbox <- st_as_sf(st_as_sfc(st_bbox(Nbox, crs = utm)))
Nsfbox$label <- 'C'

Sbox <- c(ymin = min(S$NORTHING) - 1000, 
          ymax = max(S$NORTHING) + 1000,
          xmin = min(S$EASTING) - 1000, 
          xmax = max(S$EASTING))
Ssfbox <- st_as_sf(st_as_sfc(st_bbox(Sbox, crs = utm)))
Ssfbox$label <- 'D'


# Base islands
(gfogo <- ggplot(islands) + 
    geom_sf(fill = '#d0c2a9') + 
    themeMap)

# Histogram
(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID),
                   binwidth = 10) +
    guides(fill = FALSE) +
    scale_fill_viridis_d() +
    geom_vline(aes(xintercept = 90)) +
    geom_vline(aes(xintercept = 365)) + 
    labs(x = 'Julian Day', y = NULL) + 
    themeHist)

# Edges
(gnetN <- gfogo +
    geom_edges(data = N,
      aes(
        x = meanX,
        y = meanY,
        xend = endmeanX,
        yend = endmeanY,
        color = ANIMAL_ID
      ),
      size = 2
    ) +
    ylim(Nbox[['ymin']], Nbox[['ymax']]) +
    xlim(Nbox[['xmin']], Nbox[['xmax']]) +
  guides(color = FALSE) +
  scale_color_viridis_d() + 
  labs(x = NULL, y = NULL) + 
  themeMap)

# TODO: split scales N/S
(gnetS <- gfogo +
    geom_edges(data = S,
               aes(
                 x = meanX,
                 y = meanY,
                 xend = endmeanX,
                 yend = endmeanY,
                 color = ANIMAL_ID
               ),
               size = 2
    ) +
    ylim(Sbox[['ymin']], Sbox[['ymax']]) +
    xlim(Sbox[['xmin']], Sbox[['xmax']]) +
  guides(color = FALSE) +
  scale_color_viridis_d() + 
  labs(x = NULL, y = NULL) + 
  themeMap)

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

# (gnet <- ggplot(data = edges[season == 'icefree']) +
#     geom_sf(data = islands, fill = '#c7c0bd') +
#     geom_edges(
#       aes(
#         x = meanX,
#         y = meanY,
#         xend = endmeanX,
#         yend = endmeanY,
#         color = ANIMAL_ID
#       ),
#       size = 2
#     ) +
#     ggtitle('A)') +
#     guides(color = FALSE) +
#     scale_color_viridis_d() + 
#     labs(x = NULL, y = NULL) + 
#     themeMap)

# (gcol <- ggplot(data = edges[, .N, by = .(JDate, ANIMAL_ID)]) + 
#   geom_col(aes(JDate, N, color = ANIMAL_ID)) + 
#   guides(color = FALSE) + 
#   scale_color_viridis_d())


# (g <- gnet / ghist + 
#   plot_layout(heights = c(3, 1)))


### Output fig ----
ggsave(
  'graphics/Fig2.png',
  width = 8,
  height = 4,
  units = 'in',
  dpi = 320
)
# png("graphics/Fig2.png", width = 5000, height = 4000, units = "px", res = 600)
# g
# dev.off()


### Other figs ----
# (ghist <- ggplot(data = edges) +
#     geom_histogram(aes(JDate, fill = ANIMAL_ID)) +
#     guides(fill = FALSE) +
#     scale_fill_viridis_d() +
#     geom_vline(aes(xintercept = 90)) +
#     geom_vline(aes(xintercept = 365))+ 
#     labs(x = 'Julian Day', y = NULL) + 
#     facet_wrap(~region))

# (ghist <- ggplot(data = edges[, .N, .(ANIMAL_ID, region, season)]) +
#     geom_col(aes(N, ANIMAL_ID)) +
#     guides(fill = FALSE) +
#     labs(x = 'Count', y = NULL) + 
#     facet_wrap(season~region))


# All ids
# ggplot() + geom_sf(data = islands, fill = '#c7c0bd') +  
#   # scale_fill_manual(values = c('#d7efee', '#afa89a'), limits = c('0', '1')) +
#   geom_edges(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl)) + 
#   ylim(min(edges$NORTHING) - 1000, max(edges$NORTHING) + 1000) +
#   xlim(min(edges$EASTING) - 1000, max(edges$EASTING) + 1000) +
#   coord_sf() +
#   theme(panel.background = element_rect(fill = '#d0dee5'))
# # geom_nodes(data = net, aes(xisl, yisl, xend = xendisl, yend = yendisl))# + 
# # geom_nodetext(data = net, aes(xisl, yisl, label = name))

# By id
# (gnn <- (ggplot(data = edges) +
#             geom_sf(data = islands, fill = '#c7c0bd')) +
#   ylim(edges[, min(NORTHING) - 1000], edges[, max(NORTHING) + 1000]) +
#   xlim(edges[, min(EASTING) - 1000], edges[, max(EASTING) + 1000]) +
#   geom_edges(aes(x = EASTING,
#                  y = NORTHING,
#                  xend = endislandEAST,
#                  yend = endislandNORTH,
#                  color = ANIMAL_ID)) +
#   # guides(color = FALSE, fill = FALSE) +
#   scale_shape_manual(values = c('TRUE' = 2, 'FALSE' = 4)) +
#   # scale_color_viridis_d() +
#     facet_wrap(~season) + 
#   theme(panel.background = element_rect(fill = '#d0dee5'))
# # guides(color = FALSE, size = FALSE) +
# # geom_text(aes(x, y, xend = NULL, yend = NULL, label = label), data = labels) +
# # p
# )


