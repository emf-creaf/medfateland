plot.wswb<-function(x, type = "Runon", summaryIndex = 1, ...) {

  if(type %in% names(x$CellBalance)) y = x$CellBalance[[type]]
  else if(type %in% names(x$CellState)) y = x$CellState[[type]]
  y = y[,summaryIndex]
  spdf = SpatialPixelsDataFrame(SpatialPoints(x$coords, x$proj4string), data = data.frame(y), grid = x$grid)
  a = sf::st_as_sf(as(spdf, "SpatialPolygonsDataFrame"))
  ggplot()+geom_sf(data=a, aes(fill=y))
}
