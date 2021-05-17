extract_lc <- function(value, lc, poly) {
  if (is.na(value)) {
    extract(is.na(lc), poly, fun = mean)
  } else {
    extract(lc == value, poly, fun = mean, na.rm = TRUE)
  }
}
fogopolys <- readRDS('../caribou-swimming/output/islandsPoly.Rds')
values <- c(NA, unique(fogolc))
extracts <- lapply(values, extract_lc, fogolc, fogopolys)
names(extracts) <- paste0('prop', values)
DT <- as.data.table(extracts)
DT[, id := fogopolys$id]
DT[, area := fogopolys$area]
saveRDS(DT, '../caribou-swimming/output/fogo-prop-lc.Rds')