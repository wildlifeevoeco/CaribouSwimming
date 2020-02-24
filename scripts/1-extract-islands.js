// Extract islands with SENTINEL2
// Alec Robitaille

var geometry = ee.Geometry.Polygon(
        [[[-54.45048689517963, 49.760281833675236],
          [-54.45048689517963, 49.49787241520817],
          [-53.984941240882755, 49.49787241520817],
          [-53.984941240882755, 49.760281833675236]]], null, false);
          
var col = ee.ImageCollection('COPERNICUS/S2')
  .filterDate('2019-07-01', '2019-10-01')
  .filterBounds(geometry);
  
var value = function(img) {
  return(img.addBands(img.divide(10000)
                         .select('B4', 'B3', 'B2')
                         .rgbToHsv()
                         .select('value')));
};

var dark = function(img) {
  return(img.addBands(ee.Image(1).divide(img.select('value')).rename(['dark'])));
};

var qual = col.map(value)
              .map(dark)
              .qualityMosaic('dark');

var land = qual.select('B8').gt(350);

Map.addLayer(col);
Map.addLayer(qual);
Map.addLayer(land);


Export.image.toDrive({
  image: land,
  description: 'caribouswimming',
  scale: 20,
  fileNamePrefix: 'fogo-land-caribou-swimming',
  region: geometry,
  crs: 'EPSG:32621'
});