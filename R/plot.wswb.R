plot.wswb<-function(x, type = "Runon", summaryIndex = 1, ...) {

  if(type %in% names(x$CellBalance)) y = x$CellBalance[[type]]
  else if(type %in% names(x$CellState)) y = x$CellState[[type]]
  # if(type %in% c("DI","Transpiration")) {
  #   y = y[, summaryIndex, spIndex]
  # } else {
    y = y[,summaryIndex]
  # }
  spplot(SpatialPixelsDataFrame(SpatialPoints(x$coords, x$proj4string), data = data.frame(y), grid = x$grid),...)
}
