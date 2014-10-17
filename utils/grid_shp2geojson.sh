
###
### run this script as "recreation" user!
###    ie, first: su - recreation
###

## for each directory...
for D in /usr/local/public_html/data/*;  do
  ## continue only if it's a directory
  [ -d "${D}" ] || continue
  ## store the sessionID
  BD=$(basename $D)
  echo $BD
  ## check if there's already a geojson created
  if [ ! -f "/mnt/recreation/invest_outputs_geojson/"$BD".geojson" ]
    then
      ## check if invest run succeeded in creating full grid
      if [ -f $D"grid_private.shp" -a -f $D"grid_private.prj" -a -f $D"grid_private.shx" -a -f $D"grid_public.dbf" ]
        then
          echo $BD "converted"
          cp $D"grid_private.shp" grid.shp
          cp $D"grid_private.prj" grid.prj
          cp $D"grid_private.shx" grid.shx
          cp $D"grid_public.dbf" grid.dbf
          ogr2ogr -f GeoJSON -t_srs crs:84 "/mnt/recreation/invest_outputs_geojson/"$BD".geojson" grid.shp
          rm grid.*
        else
          echo $BD "skipped"
      fi
    else
      echo $BD "exists"
  fi
done

