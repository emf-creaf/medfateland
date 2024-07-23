#' Fire regime instance 
#' 
#' Applies a fire regime object over a set of landscape units to determine a fire realization
#'
#' @param sf An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{represented_area_ha}: Area represented by each stand (in hectares).}
#'     \item{\code{ignition_weights}: Relative weights to determine stands to be burned (optional).}
#'   }
#' @param fire_regime A list of parameters defining the fire regime (see \code{\link{create_fire_regime}}).
#' 
#' @details
#' The function randomly determines the landscape units that will burn every year, depending on the specifications
#' of the fire regime object. Users can define their own fire regime instances from other models (e.g. a fire landscape model)
#' and then use those directly in functions \code{\link{fordyn_spatial}} or \code{\link{fordyn_scenario}}.
#' 
#' @return An integer matrix specifying the day of the year of burning of each landscape unit for every year in the fire regime definition.
#' Values are interpreted as follows:
#' \itemize{
#'  \item{NA - No wildfire this year}
#'  \item{0 - Wildfire will occur the driest day (i.e. the one with largest vapor pressure deficit).}
#'  \item{1...366 - Day of the year when wildfire will occur}
#' } 
#'
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{create_fire_regime}}, \code{\link{fordyn_spatial}}, \code{\link{fordyn_scenario}}
#' 
#' @examples 
#' # Load example data
#' data("example_ifn")
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' example_ifn$represented_area_ha <- 100
#' 
#' # Define fire regime characteristics
#' reg1 <- create_fire_regime(c("2002" = 200, "2003" = 500)) 
#' 
#' # Create a fire regime instance
#' m1 <- fire_regime_instance(example_ifn, reg1)
#' 
#' # Check number of plots burned
#' colSums(!is.na(m1))
#' 
#' # Define fire regime characteristics with stochastic area burned
#' reg2 <- create_fire_regime(annual_burned_area = c("2002" = 200, "2003" = 500),
#'                            sd_burned_area = c("2002" = 0.4, "2003" = 0.5)) 
#' 
#' # Create a fire regime instance
#' m2 <- fire_regime_instance(example_ifn, reg2)
#' 
#' # Check number of plots burned
#' colSums(!is.na(m2))
#' 
#' @export
fire_regime_instance <-function(sf, fire_regime) {
  
  if(!inherits(sf, "sf")) stop("'sf' has to be an object of class 'sf'.")
  if(!inherits(fire_regime, "fire_regime")) stop("'fire_regime' has to be an object of class 'fire_regime'.")
  if(!("id" %in% names(sf))) stop("Column 'id' must be defined.")
  if(!("represented_area_ha" %in% names(sf))) stop("Column 'represented_area_ha' must be defined.")
  
  years <- names(fire_regime$annual_burned_area)
  ids <- sf$id
  min_area <- min(sf$represented_area_ha)
  n <- length(ids)
  
  m <- matrix(NA, nrow = n, ncol = length(years))
  rownames(m) <- ids
  colnames(m) <- years
  for(iy in 1:length(years)) {
    target <- as.numeric(fire_regime$annual_burned_area[iy])
    sd <- as.numeric(fire_regime$sd_burned_area[iy])
    if(!is.na(sd)) {
      target <- rlnorm(1, log(target), sd)
    }
    available <- 1:n
    weights <- rep(1.0, n)
    if("ignition_weights" %in% names(sf)) {
      weights <- sf$ignition_weights
    }
    while((target >= min_area) && (length(available) > 0)) {
      if(length(available)==1) {
        s <- available
      } else {
        s <- sample(available, 1, prob = weights)
      }
      if(is.null(fire_regime$doy)) {
        m[s, iy] <- 0
      } else {
        m[s, iy] <- as.numeric(fire_regime$doy[iy])
      }
      weights <- weights[ available!= s]
      available <- available[ available!= s]
      target <- target - sf$represented_area_ha[s]
    }
    if(length(available)==0) warning(paste0("All forest stands selected for year ", years[iy], " (possibly not fulfilling burned area demand)"))
  }
  return(m)
}
