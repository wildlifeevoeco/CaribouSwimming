### Extract OSM data ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c('data.table',
          'ggplot2',
          'raster',
          'sf',
          'rosm',
          'stars',
          'fasterize')
lapply(libs, require, character.only = TRUE)

### Extract islands from OSM ----
# Set min max coords
coords <- c(ymin = 49.5194,
            ymax = 49.65,
            xmin = -54.3533,
            xmax = -54.1878)
latlon <- st_crs(4326)
bb <- bbox(matrix(c(coords[['xmin']], coords[['xmax']], 
                    coords[['ymin']], coords[['ymax']]), 
                  nrow = 2))
utm <- CRS('+proj=utm +zone=21 ellps=WGS84')
coordsOSM <- osm.raster(bb,
                        projection = utm,
                        crop = TRUE)

coordsOSM2 <- coordsOSM[[1]] != 170
coordsOSM2[coordsOSM2 == 0] <- NA
coordsOSM3 <- st_as_sf(
  st_as_stars(coordsOSM),
  as_points = FALSE,
  use_integer = TRUE,
  merge = TRUE,
  na.rm = TRUE
)
coordsOSM3$island <- seq_along(coordsOSM3$geometry)

r <- raster(
  ymn = st_bbox(coordsOSM3)$ymin,
  ymx = st_bbox(coordsOSM3)$ymax,
  xmn = st_bbox(coordsOSM3)$xmin,
  xmx = st_bbox(coordsOSM3)$xmax,
  crs = CRS('+proj=utm +zone=21 ellps=WGS84')
)
r2 <- fasterize(coordsOSM3, r, field = "island")

saveRDS(coordsOSM3, "output/coordsOSM.RDS")
saveRDS(r2, "output/islandRaster.RDS")

