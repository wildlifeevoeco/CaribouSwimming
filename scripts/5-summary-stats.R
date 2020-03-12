### Summary statistics ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c(
  'data.table'
)
lapply(libs, require, character.only = TRUE)


### Data ----
edges <- readRDS('output/island-edges.Rds')


### 
