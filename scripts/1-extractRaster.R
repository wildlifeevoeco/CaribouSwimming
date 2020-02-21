### Extract OSM data ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c('data.table',
          'ggplot2',
          'raster',
          'sf',
          'rosm',
          'stars',
          'mapview',
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

# zzz
utmbb <- projectExtent(raster(ext = extent(bb), crs = latlon$proj4string), 
                       crs = utm)
r2 <- fasterize(coordsOSM3, utmbb, field = "island")

saveRDS(coordsOSM3, "output/coordsOSM.Rds")
saveRDS(r2, "output/islandRaster.Rds")

