#' Updates the state of a landscape object
#' 
#' Updates the state of a spatial object 'x' according to the final state in simulation outcome 'y' 
#' 
#' @param x An object of class \code{\link{sf}} with the corresponding landscape columns.
#' @param y The object resulting of a simulation previously carried on \code{x} 
#'    using \code{\link{spwb_spatial}}, \code{\link{growth_spatial}}, \code{\link{spwb_land}}, etc.
#' 
#' @return An object of class \code{\link{sf}} with modified state variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{spwb_spatial_day}}, \code{\link{spwb_land}}
#' 
update_state<-function(x, y) {
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  if(!inherits(y, "sf") && !inherits(y, "spwb_land") && !inherits(y, "growth_land")) stop("'y' should be of class 'sf', 'spwb_land', or 'growth_land'")
  if(inherits(y, c("spwb_land", "growth_land"))) {
    y = y$sf
  }
  n = length(x$forest)
  if(!is.null(y$state)) {
    if(length(y$state)!=n) stop("'state' in 'y' does not have the same length as the elements in 'x'")
    for(i in 1:n) {
      if(!is.null(y$state[[i]])) {
        x$state[[i]] = y$state[[i]]
        if("soil" %in% names(y$state[[i]])) x$soil[[i]] = y$state[[i]]$soil
      }
    }
  }
  if("forest" %in% names(y)) {
    if(!is.null(y$forest)) {
      if(length(y$forest)!=n) stop("'forest`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        if(!is.null(y$forest[[i]]))  x$forest[[i]] = y$forest[[i]]
      }
    }
  }
  if("aquifer" %in% names(y)) {
    if(!is.null(y$aquifer)) {
      if(length(y$aquifer)!=n) stop("'aquifer`' in 'y' does not have the same length as the elements in 'x'")
      x$aquifer = y$aquifer
    }
  }
  if("snowpack" %in% names(y)) {
    if(!is.null(y$snowpack)) {
      if(length(y$snowpack)!=n) stop("'snowpack`' in 'y' does not have the same length as the elements in 'x'")
      x$snowpack = y$snowpack
    }
  }
  return(x)
}