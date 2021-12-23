plot.spwbland<-function(x, type = "Runon", summaryIndex = 1, ...) {
  if(type=="DailyRunoff") {
    df = data.frame(Date = as.Date(row.names(x$DailyRunoff)), 
                    Runoff = rowSums(x$DailyRunoff, na.rm=T))
    g1<-ggplot(df)+
      geom_line( aes(x=Date, y=Runoff), col="blue")+
      ylab("Watershed runoff (m3/day)")+theme_bw()
  } else if (type %in% c(names(x$CellBalance), names(x$CellState))) {
    if(type %in% names(x$CellBalance)) y = x$CellBalance[[type]]
    else if(type %in% names(x$CellState)) y = x$CellState[[type]]
    y = y[,summaryIndex]
    spdf = SpatialPixelsDataFrame(SpatialPoints(x$coords, x$proj4string), data = data.frame(y), grid = x$grid)
    a = sf::st_as_sf(as(spdf, "SpatialPolygonsDataFrame"))
    g1<-ggplot()+geom_sf(data=a, aes(fill=y))+theme_bw()
  } else {
    stop("Unknown plot type")
  }
  return(g1)
}
plot.growthland<-function(x, type = "Runon", summaryIndex = 1, ...) {
  return(plot.spwbland(x = x, type = type, summaryIndex = summaryIndex, ...))
}
