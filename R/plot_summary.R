.getSummaryMatrixVariable<-function(x, variable, date) {
  n <- length(x$summary)
  vec <- rep(NA, n)
  for(i in 1:n) {
    M = x$summary[[i]]
    if(!is.null(M)) {
      if(is.matrix(M)) {
        if(variable %in% colnames(M)) vec[i] = M[date, variable]
      }
    }
  }
  return(vec)
}
.getSummaryMatrixVarNames<-function(x) {
  n <- length(x$summary)
  vec <- rep(NA, n)
  cn <- character(0)
  for(i in 1:n) {
    M = x$summary[[i]]
    if(!is.null(M)) {
      if(is.matrix(M)) cn = unique(c(cn, colnames(M)))
    }
  }
  return(cn)
}
.getSummaryMatrixDates<-function(x) {
  n <- length(x$summary)
  vec <- rep(NA, n)
  for(i in 1:n) {
    M = x$summary[[i]]
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
#' @param x An object of class \code{sf}, with simulation summaries.
#' @param variable The variable to be drawn.
#' @param date The date of the summary to be plotted.
#' @param ... Additional parameters (passed to scale definition, such as \code{limits}).
#' 
#' @details Appropriate values for \code{x} can originate from calls to \code{\link{simulation_summary}}. 
#' Alternatively, if summary functions were specified at the time of performing simulations, 
#' the result of the spatial simulation function (e.g. \code{\link{spwb_spatial}}) 
#' will already contain the summaries.
#' 
#' @return An object of class \code{\link{ggplot}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{summary_spatial}}
plot_summary<-function(x, variable, date, ...) {
  if(!inherits(x, "sf")) stop("'x' has to be an object of class 'sf'.")
  if(!("summary" %in% names(x))) stop("Column 'summary' must be defined in 'x'.")
  match.arg(variable, .getSummaryMatrixVarNames(x))
  match.arg(date, .getSummaryMatrixDates(x))
  vec <- .getSummaryMatrixVariable(x, variable, date)
  
  df = sf::st_sf(sf::st_geometry(x), y = vec)
  g<-ggplot()+geom_sf(data=df, aes(col=.data$y))+
    scale_color_continuous("", ...)+
    labs(title = paste0(variable, " [", date,"]"))+
    theme_bw()
  return(g)
}