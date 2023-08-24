#' Fire regime instance 
#' 
#' Applies a fire regime object over a set of landscape units to determine a fire realization
#'
#' @param sf An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{represented_area}: Area represented by each stand (in hectares).}
#'     \item{\code{ignition_weights}: Relative weights to determine stands to be burned (optional).}
#'   }
#' @param fire_regime A list of parameters defining the fire regime (see \code{\link{create_fire_regime}}).
#'
#' 
#' @return An integer matrix specifying the day of the year of burning of each landscape unit for every year in the fire regime definition
#'
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{create_fire_regime}}
#' 
#' @examples 
#' 
#' @export
fire_regime_instance <-function(sf, fire_regime) {
}
