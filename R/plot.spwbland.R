plot.spwbland<-function(x, variable = "Runon", date = NULL, ...) {
  if(variable=="DailyRunoff") {
    df = data.frame(Date = as.Date(row.names(x$DailyRunoff)), 
                    Runoff = rowSums(x$DailyRunoff, na.rm=T))
    g1<-ggplot(df)+
      geom_line( aes_string(x="Date", y="Runoff"), col="blue")+
      ylab("Watershed runoff (m3/day)")+theme_bw()
  } else if (variable %in% c(colnames(x$summarylist[[1]]))) {
    g1 = plot.summarypixels(x, variable, date, ...)
  } else {
    stop("Unknown plot variable")
  }
  return(g1)
}
plot.growthland<-function(x, variable = "Runon", date = NULL, ...) {
  return(plot.spwbland(x = x, variable = variable, date = date, ...))
}
