
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
fogopolys <- readRDS('output/islandsPoly.Rds')
fogolc <- raster('../nl-landcover/output/fogo_lc.tif')

## extract proportion of habitat type by island
values <- c(NA, unique(fogolc))
extracts <- lapply(values, extract_lc, fogolc, fogopolys)
names(extracts) <- paste0('prop', values)
DT <- as.data.table(extracts)
DT[, id := fogopolys$id]
DT[, area := fogopolys$area]
saveRDS(DT, 'output/fogo-prop-lc.Rds')

## load data
DT <- readRDS('output/fogo-prop-lc.Rds')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')

setnames(DT, c("prop1", "prop2", "prop3", "prop4",
               "prop5", "prop6", "prop7", "prop8",
               "prop9", "propNA"),
         unique(legend$Cover))

DT <- DT[id == 120 | id == 128 | id == 124 |
         id == 75 | id == 70 | id == 125  |
         id == 78 | id == 73 | id == 58 | id == 72]

## change names of islands ids to island names 
DT$id[DT$id == 58] <- "Island 6"
DT$id[DT$id == 72] <- "Island 7"
DT$id[DT$id == 70] <- "Island 1"
DT$id[DT$id == 73] <- "Island 2"
DT$id[DT$id == 75] <- "Island 3"
DT$id[DT$id == 78] <- "Island 4"
DT$id[DT$id == 125] <- "Island 5"
DT$id[DT$id == 120] <- "Fogo"
DT$id[DT$id == 124] <- "E. Perry"
DT$id[DT$id == 128] <- "W. Perry"

DT$other <- DT$Broadleaf +DT$MixedWood + DT$Anthro + DT$NotAvail
DT$conifer <- DT$ConiferScrub + DT$ConiferForest

DT$areakm2 <- DT$area/1000000
