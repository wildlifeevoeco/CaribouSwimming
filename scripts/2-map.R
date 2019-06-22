
library(sf)
library(ggplot2)
library(osmdata)


FogoCaribou <- FogoCaribou[EASTING > 650000 & EASTING < 800000 &
                             NORTHING > 5470000  & NORTHING < 5520000]

bbox_to_string(getbb, "")

fogo <- st_read(some shapefile)
islands <- gDissolve(to separate islands)
islands$area <- gArea(islands, byid = TRUE)
sub <- islands[islands$area > 10km2]

## Map 
osmdata_sf()

ggplot(sub) +
  geom_sf() +
  theme(background.color = some nice light grey/blue)


