// var seatemp = ee.ImageCollection("HYCOM/sea_temp_salinity")
var seatemp = ee.ImageCollection('NOAA/CDR/OISST/V2')


var imgs = seatemp
.filter(ee.Filter.dayOfYear(90, 365))
.filterBounds(geometry)//.limit(1).aside(print)
.filter(ee.Filter.date('2016-01-01', '2020-01-01'))

Map.addLayer(imgs.limit(1))


var red = imgs.map(function(img) {
  return(img.reduceRegions(geometry, ee.Reducer.mean()))
})

print(red)

var chart = ui.Chart.image.series({
  imageCollection: imgs.select('sst'),
  region: geometry,
  reducer: ee.Reducer.mean(),
  scale: 200
});

var years = ui.Chart.image.doySeriesByYear({
  imageCollection: imgs,
  bandName: 'sst',
  region: geometry,
  scale: 200
});
print(years)
print(chart)