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
update_landscape<-function(x, y) {
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  if(!inherits(y, "sf") && !inherits(y, "spwb_land") && !inherits(y, "growth_land")) stop("'y' should be of class 'sf', 'spwb_land', or 'growth_land'")
  if(inherits(y, c("spwb_land", "growth_land"))) {
    y = y$sf
  }
  if(nrow(y)!=nrow(x)) stop("'y' does not have the same number of elements as 'x'")
  if("state" %in% names(y)) {
    x$state = y$state
    for(i in 1:nrow(x)) {
      if(!is.null(x$state[[i]])) {
        if("soil" %in% names(x$state[[i]])) x$soil[[i]] = x$state[[i]]$soil
      }
    }
  } 
  if("forest" %in% names(y)) x$forest = y$forest
  if("managementarguments" %in% names(y)) x$managementarguments = y$managementarguments
  if("aquifer" %in% names(y)) x$aquifer = y$aquifer
  if("snowpack" %in% names(y)) x$snowpack = y$snowpack
  return(x)
}
