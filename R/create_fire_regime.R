#' Create fire regime 
#' 
#' Defines an object containing fire regime parameters for simulations of forest dynamics.
#'
#' @param annual_burned_area A named vector of burned area in hectares for simulation years.
#' @param sd_burned_area A named vector of standard deviation (in log scale) of burned area. If specified, annual target to burn will be determined
#'                       using a log-normal distribution with mean values given by \code{annual_burned_area}.
#' @param doy A named integer vector with the day of the year (i.e. between 1 and 366) when fires will be simulated for each simulation year in \code{annual_burned_area}. If NULL
#'            fires will be simulated on the driest day (i.e. when vapor pressure deficit is largest).
#'
#' 
#' @details
#' Names of \code{annual_burned_area} should be simulation years. If provided, \code{sd_burned_area} should be a vector of the same size as
#' \code{annual_burned_area} and have the same names.
#' 
#' @return A list with the supplied parameters
#'
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{fire_regime_instance}}, \code{\link{fordyn_scenario}}, \code{\link{fordyn_spatial}}
#' 
#' @examples 
#' # Fire regime with pre-defined burned area values
#' reg1 <- create_fire_regime(annual_burned_area = c("2002" = 1000, "2003" = 5000)) 
#' 
#' # Fire regime with log-normal distribution for burned area
#' reg2 <- create_fire_regime(annual_burned_area = c("2002" = 1000, "2003" = 5000),
#'                            sd_burned_area = c("2002" = 0.9, "2003" = 0.8)) 
#' 
#' @export
create_fire_regime<-function(annual_burned_area,
                             sd_burned_area = NULL,
                             doy = NULL) {
  if(!is.null(sd_burned_area)) {
    if(length(sd_burned_area) != length(annual_burned_area)) {
      stop("Vectors of burned area mean and sd values should be of the same size")
    }
    if(!all(names(annual_burned_area)==names(sd_burned_area))) {
      stop("Vectors of burned area mean and sd values should have the same names (years)")
    }
  } else {
    sd_burned_area <- rep(NA, length(annual_burned_area))
    names(sd_burned_area) <- names(annual_burned_area)
  }
  if(is.null(doy)) {
    doy <- rep(0, length(annual_burned_area))
    names(doy) <- names(annual_burned_area)
  }
  l <- list(annual_burned_area = annual_burned_area,
            sd_burned_area = sd_burned_area, 
            doy = doy)
  class(l) <- c("fire_regime", "list")
  return(l)
}
