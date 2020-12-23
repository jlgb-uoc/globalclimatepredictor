Debido al tamaño de los ficheros, no se pueden alojar en GitHub.

El mapa de elevacion (alwdgg.tif) puede descargarse de esta direccion:

https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5/zipped-dem-geotiff-raster-geographic-tag-image-file-format-raster-data/zipped-dem-geotiff-raster-geographic-tag-image-file-format-raster-data/at_download/file

El mapa de distancias al mar (distSea.tif) generado mediante GRASS puede descargarse de esta direccion:

https://www.kaggle.com/joseluisgarciabravo/geographic-distance-to-the-sea

El codigo de GRASS utilizado para generar este fichero es el siguiente:

```
r.in.gdal alwdgg.tif output=alwdgg
g.region rast=alwdgg
r.mapcalc expression="sea=if(alwdgg<0,1,null())"
r.grow.distance sea distance=distSea metric=geodesic
r.out.gdal distSea output=distSea.tif
```
