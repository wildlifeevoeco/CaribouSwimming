### Summary statistics ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table'
)
lapply(libs, require, character.only = TRUE)


### Data ----
edges <- readRDS('output/island-edges.Rds')

icefree <- edges[season == 'icefree']

### Stats ----
## Only icefree
# Number of events
icefree[, .N]

# Number of individuals
icefree[, uniqueN(ANIMAL_ID)]

# Number of events by individual
icefree[, .N, ANIMAL_ID]

## Full
# Number of events by individual + icefree season
edges[, .N, .(ANIMAL_ID, season)]
