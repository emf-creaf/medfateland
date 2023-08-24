#' Create fire regime
#' 
#' Defines a fire regime parameters for simulations of forest dynamics 
#'
#' @param annual_burned_area A named vector of burned area in hectares for simulation years.
#' @param doy A named integer vector with the day of the year when fires will be simulated for simulation years. If NULL
#'            fires will be simulated on the day with worse weather conditions.
#' @param stochastic A logical flag to allow for stochasticity. If FALSE, the values in \code{annual_burned_area} are
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
  l <- list(annual_burned_area = annual_burned_area, 
            doy = doy,
            stochastic = stochastic)
  class(l) <- c("fire_regime", "list")
  return(l)
}
