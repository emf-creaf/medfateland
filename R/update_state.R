#' Updates the state of a landscape object
#' 
#' Updates the state of a spatial object 'x' according to the final state in simulation outcome 'y' 
#' 
#' @param x An object of class \code{\link{sf}} with the corresponding landscape columns.
#' @param y The object resulting of a simulation previously carried on \code{x}.
#' 
#' @return An object of class \code{\link{sf}} with modified state variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{spwb_spatial_day}}, \code{\link{spwb_land}}
#' 
update_state<-function(x, y) {
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  if(!inherits(y, "sf")) stop("'y' should be of class 'sf'")
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
  if(inherits(y, c("spwbland", "growthland"))) {
    if(!is.null(y$aquifer)) {
      if(length(y$aquifer)!=n) stop("'aquifer`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        x@aquifer[i] = y$aquifer[i]
      }
    }
    if(!is.null(y$snowpack)) {
      if(length(y$snowpack)!=n) stop("'snowpack`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        x@snowpack[i] = y$snowpack[i]
      }
    }
  }
  return(x)
}