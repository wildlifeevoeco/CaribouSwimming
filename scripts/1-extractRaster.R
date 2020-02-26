### Extract OSM data ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
libs <- c('data.table',
          'ggplot2',
          'raster',
          'sf',
          'osmdata',
          'stars',
          'fasterize')
lapply(libs, require, character.only = TRUE)

### Extract islands from OSM ----
# Set up bounding box
bb <- c(xmin = -54.3533,
        ymin = 49.5194,
        xmax = -53.954220,
        ymax = 49.763834)

# Projections
latlon <- st_crs(4326)
utm <- CRS('+proj=utm +zone=21 ellps=WGS84')

# Download osm in bbox as raster stack
coordsOSM <- osm.raster(bb,
                        projection = utm,
                        # crop = TRUE, 
                        zoomin = 1)
layer1 <- coordsOSM[[1]]
islands <- (round(layer1) != 170)
islands[islands == 0] <- NA

# Convert to an sf object
sfislands <- st_as_sf(
  st_as_stars(islands),
  as_points = FALSE,
  use_integer = TRUE,
  merge = TRUE,
  na.rm = TRUE
)
sfislands$island <- seq_along(sfislands$geometry)

# Project to UTM
utmbb <- projectExtent(raster(ext = extent(bb), crs = latlon$proj4string), 
                       crs = utm)
out <- fasterize(sfislands, utmbb, field = "island")

### Output ----
saveRDS(sfislands, "output/islandsPoly.Rds")
saveRDS(out, "output/islandsRaster.Rds")

