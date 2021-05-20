
library(sf)
library(rgdal)
library(raster)
library(data.table)

extract_lc <- function(value, lc, poly) {
  if (is.na(value)) {
    extract(is.na(lc), poly, fun = mean)
  } else {
    extract(lc == value, poly, fun = mean, na.rm = TRUE)
  }
}

## load data
fogopolys <- readRDS('../caribou-swimming/output/islandsPoly.Rds')
fogolc <- raster('../fogo_coyote_repeat/data/raw-data/Landcover/FogoSDSS_RS.tif')

## extract proportion of habitat type by island
values <- c(NA, unique(fogolc))
extracts <- lapply(values, extract_lc, fogolc, fogopolys)
names(extracts) <- paste0('prop', values)
DT <- as.data.table(extracts)
DT[, id := fogopolys$id]
DT[, area := fogopolys$area]
saveRDS(DT, '../caribou-swimming/output/fogo-prop-lc.Rds')

## load data
DT <- readRDS('../caribou-swimming/output/fogo-prop-lc.Rds')
legend <- fread('../fogo_coyote_repeat/data/raw-data/Landcover/Legend.csv')

DT[is.na(DT)] <- 0

setnames(DT, c("prop1", "prop2", "prop3", "prop4",
               "prop5", "prop6", "prop7", "prop8",
               "prop9", "propNA"),
         unique(legend$Cover))


