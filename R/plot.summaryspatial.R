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

#' Displays spatial simulation summaries
#' 
#' Produces graphical output of the summaries of a simulation models
#' 
#' @param x An object of class \code{summaryspatial}, with simulation summaries.
#' @param variable The variable to be drawn.
#' @param date The date of the summary to be plotted.
#' @param ... Additional parameters (passed to scale definition, such as \code{limits}).
#' 
#' @details Appropriate values for \code{x} can originate from calls to summary functions (e.g. \code{\link{summary.spwbspatial}}). 
#' Alternatively, if summary functions were specified at the time of performing simulations, 
#' the result of the simulation function (e.g. \code{\link{spwbspatial}}) will already contain the summaries.
#' 
#' @return An object of class \code{\link{ggplot}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwbspatial}}, \code{\link{summary.spwbspatial}}
plot.summaryspatial<-function(x, variable, date, ...) {
  match.arg(variable, .getSummaryMatrixVarNames(x))
  match.arg(date, .getSummaryMatrixDates(x))
  vec <- .getSummaryMatrixVariable(x, variable, date)
  
  df = data.frame(y = vec)
  if(inherits(x$sp, c("SpatialPoints"))) {
    a = sf::st_as_sf(SpatialPointsDataFrame(x$sp, data = df))
    g1<-ggplot()+geom_sf(data=a, aes_string(col="y"))+
      scale_color_continuous("", ...)
  } else {
    spdf = SpatialPixelsDataFrame(as(x$sp, "SpatialPoints"), data = df, 
                                  grid = x$sp@grid)
    a = sf::st_as_sf(as(spdf, "SpatialPolygonsDataFrame"))
    g1<-ggplot()+geom_sf(data=a, aes_string(fill="y"))+
      scale_fill_continuous("", ...)
  }
  g1<-g1+
    labs(title = paste0(variable, " [", date,"]"))+
    theme_bw()
  return(g1)
}