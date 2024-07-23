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
.getSummaryScale<-function(variable, fill, ...) {
  if(fill) {
    scale <- scale_fill_continuous("", ..., na.value = NA)
  } else {
    scale <- scale_color_continuous("", ..., na.value = NA)
  }
  if(variable %in% c("InterflowBalance", "BaseflowBalance")) {
    if(fill) {
      scale <- scale_fill_gradient2("", low = "brown", mid = "white", high = "blue",..., na.value = NA)
    } else {
      scale <- scale_color_gradient2("", low = "brown", mid = "white", high = "blue",..., na.value = NA)
    } 
  }
  return(scale)
}
  

#' Displays spatial simulation summaries
#' 
#' Produces graphical output of the summaries of a simulation models
#' 
#' @param x An object of class \code{\link[sf]{sf}}, with simulation summaries.
#' @param variable The variable to be drawn.
#' @param date The date of the summary to be plotted.
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' @param ... Additional parameters (passed to scale definition, such as \code{limits}).
#' 
#' @details Appropriate values for \code{x} can originate from calls to \code{\link{simulation_summary}}. 
#' Alternatively, if summary functions were specified at the time of performing simulations, 
#' the result of the spatial simulation function (e.g. \code{\link{spwb_spatial}}) 
#' will already contain the summaries. A special case is made for \code{\link{spwb_land}} and \code{\link{growth_land}}, 
#' that are accepted inputs as \code{x}, because its element 'sf' is used.
#' 
#' @return An object of class \code{\link[ggplot2]{ggplot}}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{simulation_summary}}
#' @export
plot_summary<-function(x, variable, date, r = NULL, ...) {
  if(inherits(x, "spwb_land") || inherits(x, "growth_land"))  x <- x$sf
  if(!inherits(x, "sf")) stop("'x' has to be an object of class 'sf'.")
  if(!("summary" %in% names(x))) stop("Column 'summary' must be defined in 'x'.")
  match.arg(variable, .getSummaryMatrixVarNames(x))
  match.arg(date, .getSummaryMatrixDates(x))
  vec <- .getSummaryMatrixVariable(x, variable, date)
  
  df = sf::st_sf(sf::st_geometry(x), y = vec)
  if(is.null(r)) {
    g<-ggplot()+geom_sf(data=df, aes(col=.data$y))+
      .getSummaryScale(variable, fill = FALSE, ...)+
      labs(title = paste0(variable, " [", date,"]"))+
      theme_bw()
  } else {
    sf_coords <- sf::st_coordinates(x)
    sf2cell <- terra::cellFromXY(r, sf_coords)
    if(any(is.na(sf2cell))) cli::cli_abort("Some coordinates are outside the raster definition.")
    if(length(sf2cell)!=length(unique(sf2cell))) cli::cli_abort("Only one element in 'sf' is allowed per cell in 'r'.")
    raster_var <- r
    m1 <- rep(NA, terra::ncell(r))
    m1[sf2cell] <- vec
    raster_var$m1 <- m1
    g<-ggplot()+
      tidyterra::geom_spatraster(aes(fill=.data$m1), data = raster_var)+
      .getSummaryScale(variable, fill=TRUE, ...)+
      labs(title = paste0(variable, " [", date,"]"))+
      theme_bw()
  }
  return(g)
}