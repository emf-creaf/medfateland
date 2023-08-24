#' Create fire regime 
#' 
#' Defines an object containing fire regime parameters for simulations of forest dynamics.
#'
#' @param annual_burned_area A named vector of burned area in hectares for simulation years.
#' @param doy A named integer vector with the day of the year (i.e. between 1 and 366) when fires will be simulated for each simulation year in \code{annual_burned_area}. If NULL
#'            fires will be simulated on the driest day (i.e. when vapor pressure deficit is largest).
#' @param stochastic A logical flag to allow for stochastic fire simulation. If FALSE, the values in \code{annual_burned_area} are
#'                   taken directly as target area to be burned. If TRUE, the target area to be burned is assumed to follow
#'                   a Poisson distribution of lambda equal to the supplied value.
#'
#' 
#' @return A list with the supplied parameters
#'
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{fordyn_scenario}}, \code{\link{fordyn_spatial}}
#' 
#' @examples 
#' freg <- create_fire_regime(c("2002" = 1000, "2003" = 5000)) 
#' 
#' @export
create_fire_regime<-function(annual_burned_area,
                             doy = NULL,
                             stochastic = FALSE) {
  if(is.null(doy)) {
    doy <- rep(0, length(annual_burned_area))
    names(doy) <- names(annual_burned_area)
  }
  l <- list(annual_burned_area = annual_burned_area, 
            doy = doy,
            stochastic = stochastic)
  class(l) <- c("fire_regime", "list")
  return(l)
}
