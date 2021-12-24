plot.spwbland<-function(x, type = "Runon", date = NULL, ...) {
  if(type=="DailyRunoff") {
    df = data.frame(Date = as.Date(row.names(x$DailyRunoff)), 
                    Runoff = rowSums(x$DailyRunoff, na.rm=T))
    g1<-ggplot(df)+
      geom_line( aes(x=Date, y=Runoff), col="blue")+
      ylab("Watershed runoff (m3/day)")+theme_bw()
  } else if (type %in% c(colnames(x$summarylist[[1]]))) {
    g1 = plot.summarypixels(x, type, date, ...)
  } else {
    stop("Unknown plot type")
  }
  return(g1)
}
plot.growthland<-function(x, type = "Runon", date = NULL, ...) {
  return(plot.spwbland(x = x, type = type, date = date, ...))
}
