### Extract land cover ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
library(sf)
library(rgdal)
library(raster)
library(data.table)

### Functions ----
extract_lc <- function(value, lc, poly) {
  if (is.na(value)) {
    extract(is.na(lc), poly, fun = mean)
  } else {
    extract(lc == value, poly, fun = mean, na.rm = TRUE)
  }
}


### Data ----
fogopolys <- readRDS('output/islandsPoly.Rds')
fogolc <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')

### Extract proportion of habitat type by island ----
values <- c(NA, unique(fogolc))
extracts <- lapply(values, extract_lc, fogolc, fogopolys)
names(extracts) <- paste0('prop', values)

DT <- as.data.table(extracts)

DT[, id := fogopolys$id]
DT[, area := fogopolys$area]

setnames(DT, c("prop1", "prop2", "prop3", "prop4",
               "prop5", "prop6", "prop7", "prop8",
               "prop9", "propNA"),
         c(unique(legend$Landcover), "Not Available"))

selislands <- data.table(
  id = c(120, 128, 124, 75, 70, 125, 78, 73, 58, 72),
  name = c("Fogo", "W. Perry", "E. Perry", "Island 3", 
           "Island 1", "Island 5", "Island 4", 
           "Island 2", "Island 6", "Island 7")
)
DT <- DT[selislands, on = 'id']

DT[, Other := Broadleaf + `Mixed Wood`  + `Anthropogenic and disturbance` +
     `Not Available`]
DT[, Conifer := `Conifer Forest` + `Conifer Scrub`]

DT[, areakm2 := area / 1000000]


# Output
saveRDS(DT, 'output/fogo-prop-lc.Rds')
