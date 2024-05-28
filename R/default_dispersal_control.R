#' Default control parameters for dispersal
#' 
#' Defines default control parameters for dispersal process
#' 
#' @return A list with the following items:
#'  \itemize{
#'        \item{\code{distance_step  [= 25]}: Distance step in meters.}
#'        \item{\code{maximum_dispersal_distance [= 3000]}: Maximum dispersal distance in meters.}
#'        \item{\code{min_percent [= 1]}: A minimum percent of seed bank to retain entry in \code{seedBank} element of \code{forest}.}
#'        \item{\code{stochastic_resampling [= FALSE]}: A flag to indicate that stochastic resampling of stands is performed.}
#'  }
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#'
#' @seealso \code{\link{spwb_land}}, \code{\link{fordyn_scenario}} \code{\link{dispersal}}
#' 
#' @examples default_dispersal_control()
#' 
#' @export
default_dispersal_control<-function() {
  l <- list(
    distance_step = 25, 
    maximum_dispersal_distance = 3000, 
    min_percent = 1,
    stochastic_resampling = FALSE
  )
  return(l)
}