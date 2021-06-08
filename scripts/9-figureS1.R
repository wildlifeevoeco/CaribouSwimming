### Figure S1 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal

### Packages ----
libs <- c(
  'data.table',
  'sf',
  'ggsflabel',
  'ggnetwork',
  'patchwork',
  'effects',
  'ggplot2'
)
lapply(libs, require, character.only = TRUE)


### Data ----
islands <- readRDS('output/islandsPoly.Rds')
edges <- readRDS('output/island-edges.Rds')


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

# Base islands ------------------------------------------------------------
labels <- data.table(
  id = c(118, 126, 122, 75, 70, 125, 78, 73, 58, 72),
  label = c("Fogo", "W. Perry", "E. Perry", "Island 3", 
           "Island 1", "Island 5", "Island 4", 
           "Island 2", "Island 6", "Island 7")
)
selislands <- merge(islands, labels, on = 'id')


png('graphics/FigS1.png',
  width = 3000,
  height = 3000,
  units = 'px',
  res = 600)
ggplot(selislands) + 
  geom_sf(fill = islandcol, size = 0.3, color = coastcol) + 
  guides(color = FALSE) +
  labs(x = NULL, y = NULL) + 
  geom_sf_label_repel(aes(label = label), 
                      data = selislands[selislands$label != 'Fogo Island', ], 
                      nudge_x = 2.2e3, size = 2) +
  geom_sf_label(aes(label = label), 
                data = selislands[selislands$label == 'Fogo Island', ], size = 2) +
  themeMap + 
  theme(axis.text = element_text(size = 11, color = 'black'),
        axis.ticks = element_blank())
dev.off()
