### Tables ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
library(data.table)
library(raster)
library(sf)

### Data ----
lc <- raster('../nl-landcover/output/fogo_lc.tif')
islands <- readRDS('output/islandsPoly.Rds')
edges <- readRDS('output/island-edges.Rds')
caribou <- readRDS('output/islands-locs.Rds')


### Table 1 ----
only <- c(120, 128, 124)
selislands <- islands[islands$id %in% only,]

selislands$proplichen <- extract(lc == 8, selislands, fun = mean, na.rm = TRUE)

counts <- rbindlist(lapply(only, function(o) {
  data.table(island = o,
             from = edges[island == o, .N],
             to = edges[endisland == o, .N])
}))



tab1 <- data.table(
  island = c('Fogo Island', 'Western Perry Island', 'Eastern Perry Island'),
  movto = counts$to,
  movfrom = counts$from,
  proplic = round(selislands$proplichen[c(1, 3, 2)], 2),
  density = c(250, 50, 20),
  grpsize = c('3.05 (2.7, 3.4)',
              '2.95 (1.89, 4.02)',
              '3.71 (1.97, 5.46)'),
  calfcow = c('0.34, (0.28, 0.39) (n = 283 groups)',
              '0.22, (0.05, 0.39) (n = 23 groups)',
              '0.67, (0.23, 1.00) (n = 7 groups)'),
  area = c(255, 7.5, 3.8)
)

tab1[, density := density / area]
tab1[, area := NULL]

setnames(tab1,
         c('',
           'Moves to',
           'Moves away',
           'Proportion lichen',
           'Estimated density (caribou per km2)',
           'Average group size (95% CI)',
           'Calf:cow ratio between June and August'))


### Table 2 ----
# Grab unique island runs for each ID
runs <- unique(caribou[, .(ANIMAL_ID, islandrun, islandlen, island)])

# Merge with island area
runarea <- runs[data.table(islands)[, .(island = id, area)], on = 'island']

# Drop units format 
runarea[, area := as.numeric(area)]


### Output ----
saveRDS(tab1, 'output/table1.Rds')
