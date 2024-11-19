

1. `changeCRS.R` to make sure the files have matching CRSs
2. `getWSE.R` to turn the 10 m WSHs into WSEs
3. Use QGIS to make the spatial extent of the 10m WSE raster match that of `norristown_5m.tif`
4. `checkSpatialExtent` to ensure spatial extents are the same now.
5. Use CostGrow, method=area and 1 settings in QGIS to get downscaled WSE projection
6. `computeWSHfromWSE.R` to convert the WSE to WSH
7. `getPerformance` to evaluate performance
