

### Packages ----
libs <- c('data.table', 'ggplot2', 'dplyr',
          'raster', 'prettymapr', 'rosm', 
          'stars', 'fasterize')
lapply(libs, require, character.only = TRUE)

### extract islands from osm 
coords <- makebbox(49.5194, -54.1878, 
                   49.65, -54.3533)

coordsOSM <- osm.raster(coords, projection=CRS('+proj=utm +zone=21 ellps=WGS84'), crop=TRUE)
coordsOSM2 <- coordsOSM[[1]] != 170
coordsOSM2[coordsOSM2 == 0] <- NA
coordsOSM3 <- st_as_stars(coordsOSM2) %>% 
  st_as_sf(as_points = FALSE, use_integer = TRUE, merge = TRUE, na.rm = TRUE)

coordsOSM3$island <- seq_along(coordsOSM3$geometry)

r <- raster(ymn = st_bbox(coordsOSM3)$ymin,
            ymx = st_bbox(coordsOSM3)$ymax, 
            xmn = st_bbox(coordsOSM3)$xmin,
            xmx = st_bbox(coordsOSM3)$xmax, 
            crs = CRS('+proj=utm +zone=21 ellps=WGS84'))
r2 <- fasterize(coordsOSM3, r, field = "island")

saveRDS(r2, "output/islandRaster.RDS")


