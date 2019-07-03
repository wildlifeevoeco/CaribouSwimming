
library(sf)
library(ggplot2)
library(osmdata)



remotes::install_github('ropensci/osmdata')

q <- opq(bbox = c(-54.35, 49.53, -53.9, 49.755))

q <- opq(bbox = 'Fogo')



FogoCaribou <- FogoCaribou[EASTING > 650000 & EASTING < 800000 &
                             NORTHING > 5470000  & NORTHING < 5520000]

bbox_to_string(getbb, "")

Fogo <- st_read("/Users/quinnwebber/Dropbox/Fogo/RSF_for_app/Code and files/FogoPoly.shp")


plot(Fogo)

st_area(Fogo)

islands <- gUnaryUnion(Fogo)
islands$area <- gArea(islands, byid = TRUE)
sub <- islands[islands$area > 10km2]

## Map 
## Not run: 
NL_sf <- opq("Newfoundland") %>%
  #add_osm_feature(key="roads", value="ruins") %>%
  osmdata_sf()

ggplot() + geom_sf(data = fortify(NL_sf))

ggplot(sub) +
  geom_sf() +
  theme(background.color = some nice light grey/blue)


