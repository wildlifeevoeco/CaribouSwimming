
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

setnames(DT, c("prop1", "prop2", "prop3", "prop4",
               "prop5", "prop6", "prop7", "prop8",
               "prop9", "propNA"),
         unique(legend$Cover))

## island ids
## Fogo = 120
## W. Perry = 128
## E. Perry = 124
## Long skinny island north of W. Perry = 75
## Island east of W. Perry = 70
## Island southwest of W. Perry = 125
## Island south of W. Perry = 78
## Island southcentre of W. Perry = 73

DT <- DT[id == 120 | id == 128 | id == 124 |
         id == 75 | id == 70 | id == 125  |
         id == 78 | id == 73]

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
