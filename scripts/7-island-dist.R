### Swim distance ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
library(data.table)
library(spatsoc)


### Data ----
edges <- readRDS('output/island-edges.Rds')
alloc.col(edges)

### Distance ----
# Generate undirected edge id
dyad_id(edges, 'island', 'endisland')

# Calculate mean step length between islands, undirected
edges[, swimdist := mean(stepLength), dyadID]

