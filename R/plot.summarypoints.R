.getSummaryMatrixVariable<-function(x, variable, date) {
  n <- length(x$summarylist)
  vec <- rep(NA, n)
  for(i in 1:n) {
    M = x$summarylist[[i]]
    if(!is.null(M)) {
      if(is.matrix(M)) {
        if(variable %in% colnames(M)) vec[i] = M[date, variable]
      }
    }
  }
  return(vec)
}
.getSummaryMatrixVarNames<-function(x) {
  n <- length(x$summarylist)
  vec <- rep(NA, n)
  cn <- character(0)
  for(i in 1:n) {
    M = x$summarylist[[i]]
    if(!is.null(M)) {
      if(is.matrix(M)) cn = unique(c(cn, colnames(M)))
    }
  }
  return(cn)
}
.getSummaryMatrixDates<-function(x) {
  n <- length(x$summarylist)
  vec <- rep(NA, n)
  for(i in 1:n) {
    M = x$summarylist[[i]]
    if(!is.null(M)) {
      if(is.matrix(M)) return(rownames(M))
    }
  }
  return(character(0))
}
.plot_result<-function(x, variable, date, ...) {
  match.arg(variable, .getSummaryMatrixVarNames(x))
  match.arg(date, .getSummaryMatrixDates(x))
  vec <- .getSummaryMatrixVariable(x, variable, date)
  
  df = data.frame(y = vec)
  row.names(df) = names(x$forestlist)
  if(inherits(x, c("summarypoints"))) {
    a = sf::st_as_sf(SpatialPointsDataFrame(x$sp, data = df))
    g1<-ggplot()+geom_sf(data=a, aes_string(col="y"))+
      scale_color_continuous("")
  } else {
    spdf = SpatialPixelsDataFrame(as(x$sp, "SpatialPoints"), data = df, 
                                  grid = x$sp@grid)
    a = sf::st_as_sf(as(spdf, "SpatialPolygonsDataFrame"))
    g1<-ggplot()+geom_sf(data=a, aes_string(fill="y"))+
      scale_fill_continuous("")
  }
  g1<-g1+
    labs(title = paste0(variable, " [", date,"]"))+
    theme_bw()
  return(g1)
}
plot.summarypoints<-function(x, variable, date, ...) {
  return(.plot_result(x,variable, date,...))
}
plot.summarygrid<-function(x, variable, date, ...) {
  return(.plot_result(x,variable, date,...))
}
plot.summarypixels<-function(x, variable, date, ...) {
  return(.plot_result(x,variable, date,...))
}