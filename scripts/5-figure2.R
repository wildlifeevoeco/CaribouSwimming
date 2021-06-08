### Figure 2 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table',
  'dplyr',
  'sf',
  'ggsflabel',
  'ggnetwork',
  'patchwork',
  'effects',
  'lme4',
  'ggplot2'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS('output/islandsPoly-with-label.Rds')
edges <- readRDS('output/island-edges.Rds')
net <- readRDS('output/island-network.Rds')
runarea <- readRDS('output/runarea.Rds')

# CRS
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')

# Drop some edges
edges <- edges[!(ANIMAL_ID == 'FO2016013' & (island == 5 | endisland == 5))]


# summary stats for % swims per island
edges[, .N, by = island] 

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

themeHist <- theme(panel.border = element_rect(size = 1, fill = NA),
                   panel.background = element_rect(fill = 'white'), 
                   axis.text = element_text(size = 11, color = 'black'),
                   axis.title = element_text(size = 12, color = 'black'))

themeRes <- theme(legend.position = 'none',
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = 'white'), 
                  axis.text = element_text(size = 11, color = 'black'),
                  axis.title = element_text(size = 12, color = 'black'))



# Wrangle edges -----------------------------------------------------------
# First 
tofirst <- c('EASTING', 'NORTHING', 'endislandEAST', 'endislandNORTH')
outfirst <- c(x = 'firstX', y = 'firstY', xend = 'endfirstX', yend = 'endfirstY')

edges[, (outfirst) := .SD, 
      by = .(island, ANIMAL_ID), .SDcols = tofirst]


# North/South regions
edges[, region := ifelse(firstY < median(firstY) + 6000, 'South', 'North')]
N <- edges[season == 'icefree' & region == 'North']
S <- edges[season == 'icefree' & region == 'South']

# Bboxes
Nbox <- c(ymin = min(N$NORTHING) + 1250, 
          ymax = max(N$NORTHING),
          xmin = min(N$EASTING) + 5000, 
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


# Base islands ------------------------------------------------------------
(gfogo <- ggplot(islands) + 
    geom_sf(fill = islandcol, size = 0.3, color = coastcol) + 
    themeMap +
    theme(axis.text = element_text(size = 11, color = 'black')) +
    scale_y_continuous(label = function(x) sprintf('%.2f°N', x)) +
    scale_x_continuous(label = function(x) sprintf('%.1f°W', -1 * x)))



# Histogram ---------------------------------------------------------------
(ghist <- ggplot(data = edges) +
    geom_histogram(aes(JDate, fill = ANIMAL_ID),
                   binwidth = 10) +
    guides(fill = FALSE) +
    scale_fill_manual(values = cols) +
    geom_vline(aes(xintercept = 90), size = 0.35) +
    geom_vline(aes(xintercept = 365), size = 0.35) + 
    labs(x = 'Calendar day', y = NULL) + 
    themeHist)


# Edges -------------------------------------------------------------------
edgesize <- 1
(gnetN <- gfogo +
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

(gnetS <- gfogo +
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

islandcents <- st_centroid(
  islands[
  !is.na(islands$label) &
  !grepl('Island ', islands$label) & 
  islands$area > units::as_units(1e5, 'm^2'),
])

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
    geom_sf_label(aes(label = label),
                  data = islandcents,
                  size = 2) +
    theme(axis.title = element_blank())
)



# Residency time ----------------------------------------------------------
# Modify DT
runarea <- na.omit(runarea)[ANIMAL_ID %in% edges$ANIMAL_ID]
runarea$area2 <- runarea$area/1000000
runarea$logIsland <- log10(runarea$islandlen)
runarea$logArea <- log10(runarea$area/1000000)

## linear relationship 
mod <- lmer(log(area2)~log(islandlen) + (1|ANIMAL_ID), data = runarea)
summary(mod)

new <- data.frame(islandlen = c(seq(0.001, 1, 0.001), seq(min(runarea$islandlen), 
                          max(runarea$islandlen))))
predicted_df <- predict(lm(area2~islandlen,
                                      data = runarea), new, se.fit = TRUE)

predicted_df <- data.table(pred = predicted_df$fit,
                           se = predicted_df$se.fit,
                           islandlen = c(seq(0.001, 1, 0.001), 
                                         seq(min(runarea$islandlen), 
                                             max(runarea$islandlen))))

predicted_df$pred <- predicted_df$pred - 35.5
predicted_df$se <- predicted_df$se - 6.9

labels <- data.frame(
  x = c(0.0025, 0.05, 5, 200),
  y = c(8, 100, 2000, 8000),
  text = c("N. Coastal Islands", "S. Coastal Islands", "Perry Islands", "Fogo Island")
)

(resTime <- ggplot() +
  geom_jitter(data = runarea, aes(area2, islandlen, color = ANIMAL_ID), 
              width = 0.1, 
              height = 0.1, 
              size = 2, 
              alpha = 0.75) +
  geom_line(data = predicted_df, aes(x = islandlen, y = pred), color='black') + 
  #geom_line(data = predicted_df, aes(x = islandlen, y = pred - se), color='black' , lty = 2) +   
  #geom_line(data = predicted_df, aes(x = islandlen, y = pred + se), color='black' , lty = 2) +
#geom_smooth(data = runarea, aes(islandlen, area2), method = "lm", color = "black") +
  scale_y_log10(limits = c(0.01, 50000),
                breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                labels = c('0.01', '0.1', '1', '10', '100', '1000')) +
  scale_x_log10(limits = c(0.0001, 1000), 
                breaks = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000),
                labels = c('0.0001', '0.001', '0.01', '0.1', '1', '10', '100', '1000')) +
  labs(y = 'Residency (days)',
       x = expression("Area of island"~(km^2))) +
  #geom_ribbon(data=demo.fits, aes(x = conc, y = p, ymin=pmin, ymax=pmax), alpha=0.2) + 
  #geom_line(data=demo.fits, aes(x = conc, y = p)) +
  scale_color_manual(values = cols) +
  geom_text(data = labels, aes(x,y, label = text)) +
  themeRes)
 
# Patchwork ---------------------------------------------------------------
layout <- 'AAAABBBB
           AAAABBBB
           AAAABBBB
           CCDDBBBB
           CCDDEEEE' 


(g <- withboxes + resTime +
   gnetN + gnetS + ghist +
   plot_layout(design = layout) +
   plot_annotation(tag_levels = 'A')
)



# Output fig --------------------------------------------------------------
ggsave(
  'graphics/Fig2.png',
  width = 24,
  height = 15,
  units = 'cm',
  dpi = 320
)
