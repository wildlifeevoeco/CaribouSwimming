### Maps ----
mapview(
  caribou[ANIMAL_ID %in% edges$ANIMAL_ID],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'island',
  crs = utm
)


mapview(
  edges,
  xcol = 'endislandEAST',
  ycol = 'endislandNORTH',
  zcol = 'endisland',
  crs = utm
)

mapview(
  edges,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'island',
  crs = utm
)

mapview(
  caribou[!is.na(island)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

mapview(
  duration,
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

mapview(
  caribou[island != 32280],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)


mapview(
  caribou[is.na(island)],
  xcol = 'EASTING',
  ycol = 'NORTHING',
  zcol = 'ANIMAL_ID',
  crs = utm
)

