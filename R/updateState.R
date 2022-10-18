#' Updates the state of a landscape object
#' 
#' Updates the state of a spatial object 'x' according to the final state in simulation outcome 'y' 
#' 
#' @param x An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}}
#' @param y The object resulting of a simulation previously carried on \code{x}.
#' 
#' @return An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}} with modified state variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwbspatial}}, \code{\link{spwbspatial_day}}, \code{\link{spwbland}}
#' 
updateState<-function(x, y) {
  if(!inherits(x, c("SpatialPointsLandscape", "SpatialPixelsLandscape", "SpatialGridLandscape"))) stop("'x' should be of class 'SpatialPointsLandscape', 'SpatialPixelsLandscape' or 'SpatialGridLandscape' ")
  if(!inherits(y, c("spwbspatial", "growthspatial", "fordynspatial",
                    "spwbspatial_day", "growthspatial_day")))  
    stop("'y' should be of class 'spwbspatial', 'growthspatial', 'fordynspatial', 'spwbspatial_day' or 'growthspatial_day'")
  n = length(x@forestlist)
  if(!is.null(y$xlist)) {
    if(length(y$xlist)!=n) stop("'xlist' in 'y' does not have the same length as the elements in 'x'")
    for(i in 1:n) {
      if(!is.null(y$xlist[[i]])) {
        x@xlist[[i]] = y$xlist[[i]]
        if("soil" %in% names(y$xlist[[i]])) x@soillist[[i]] = y$xlist[[i]]$soil
      }
    }
  }
  if(inherits(y, c("fordynpoints", "fordynpixels", "fordyngrid"))) {
    if(!is.null(y$forestlist)) {
      if(length(y$forestlist)!=n) stop("'forestlist`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        if(!is.null(y$forestlist[[i]]))  x@forestlist[[i]] = y$forestlist[[i]]
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