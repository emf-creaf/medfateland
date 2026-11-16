#' Updates the state of a landscape object
#' 
#' Updates the state of a spatial object 'x' according to the final state in simulation outcome 'y' 
#' 
#' @param x An object of class \code{\link[sf]{sf}} with the corresponding landscape columns.
#' @param y The object resulting of a simulation previously carried on \code{x} 
#'    using \code{\link{spwb_spatial}}, \code{\link{growth_spatial}}, \code{\link{spwb_land}}, etc.
#' @param columns String vector with the names of columns to be modified
#' 
#' @return An object of class \code{\link[sf]{sf}} with modified state variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{spwb_spatial_day}}, \code{\link{spwb_land}}
#' 
#' @export
update_landscape<-function(x, y, columns = c("state", "soil", "forest", "management_arguments", "aquifer", "snowpack", "outlet_backlog")) {
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  if(!inherits(y, "sf") && !inherits(y, "spwb_land") && !inherits(y, "growth_land") 
     && !inherits(y, "fordyn_land") && !inherits(y, "fordyn_scenario")) 
    stop("'y' should be of class 'sf', 'spwb_land', 'growth_land', 'fordyn_land' or 'fordyn_scenario'")
  if(inherits(y, c("spwb_land", "growth_land", "fordyn_land"))) {
    y <- y$sf
  } else if(inherits(y, c("fordyn_scenario"))) {
    y <- y$next_sf
  }
  if(nrow(y) != nrow(x)) stop("'y' does not have the same number of elements as 'x'")
  if("state" %in% columns) if("state" %in% names(y)) x$state <- y$state
  if("soil" %in% columns) {
    if("state" %in% names(y)) {
      if(!("soil" %in% names(x))) x[["soil"]] <- vector("list", nrow(x)) # initialize soil column if necessary
      for(i in 1:nrow(x)) {
        if(!is.null(y$state[[i]])) {
          if("soil" %in% names(y$state[[i]])) x$soil[[i]] <- y$state[[i]]$soil
        }
      }
    } 
  }
  if("forest" %in% columns) if("forest" %in% names(y)) x$forest <- y$forest
  if("management_arguments" %in% columns) if("management_arguments" %in% names(y)) x$management_arguments <- y$management_arguments
  if("aquifer" %in% columns) if("aquifer" %in% names(y)) x$aquifer <- y$aquifer
  if("snowpack" %in% columns) if("snowpack" %in% names(y)) x$snowpack <- y$snowpack
  if("outlet_backlog" %in% columns) if("outlet_backlog" %in% names(y)) x$outlet_backlog <- y$outlet_backlog
  # place geometry in first position
  ns <- names(x)
  if("geom" %in% ns) {
    x <- x[,c("geom", ns[!(ns %in% "geom")])]
  } else if("geometry" %in% ns) {
    x <- x[,c("geometry", ns[!(ns %in% "geometry")])]
  }
  return(x)
}
