
## grid_private.dbf did not include the PUD values, workaround:
sudo cp grid_private.shp grid.shp
sudo cp grid_private.prj grid.prj
sudo cp grid_private.shx grid.shx
sudo cp grid_public.dbf grid.dbf

## shapefile to geoJSON and transform to wgs84
sudo ogr2ogr -f GeoJSON -t_srs crs:84 grid.geojson grid.shp 

## dbf to csv because foreign::read.dbf() does not like a url pathname
sudo ogr2ogr -f CSV grid.csv grid_public.dbf

## aoi shapefile to geojson, transform, and write bbox
## may need to generalize this by first dissolving aoi.shp, in case it is multipart.
## WRITE_BBOX ought to create bbox for the entire layer, but is only creating bbox for individual polygons.
## sudo ogr2ogr -f GeoJSON -t_srs crs:84 -lco WRITE_BBOX=YES aoi.geojson aoi.shp 