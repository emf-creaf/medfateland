#' Summarizes spatial simulation results
#' 
#' Creates spatial objects containing summaries of simulations
#' 
#' @param object An object of class 'sf' simulation results (e.g. the result of calling \code{\link{spwb_spatial}}).
#' @param summary_function The summary function to be executed on simulation results (see details).
#' @param ... Additional parameters to the summary function.
#' 
#' @details The function supplied should take as input an object of local simulation function, i.e. \code{\link[medfate]{spwb}}, \code{\link[medfate]{growth}}, or \code{\link[medfate]{fordyn}}. 
#' The output should be a matrix with dates as rows and variables in columns. 
#' An example of suitable function is \code{\link[medfate]{summary.spwb}}.
#' 
#' @return An object of class \code{\link[sf]{sf}}, with the following two elements:
#'  \itemize{
#'    \item{\code{geometry}: Spatial geometry.}
#'    \item{\code{id}: Stand id, taken from the input.}
#'    \item{\code{summary}: A list of model output summaries for each simulated location.}
#'  }
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_spatial}}, \code{\link{plot_summary}}
#' 
#' @name simulation_summary
#' @export
simulation_summary<-function(object, summary_function, ...) {
  if(!inherits(object, "sf")) stop("'object' has to be an object of class 'sf'.")
  if(!("result" %in% names(object))) stop("Column 'result' must be defined in 'object'.")
  n <- length(object$result)
  summarylist <- vector("list", n)
  for(i in 1:n) {
    if(!is.null(object$result[[i]])) {
      argList <- list(object=object$result[[i]],...)
      summarylist[[i]] <- do.call(summary_function, args=argList)
    }
  }
  res <- sf::st_sf(geometry = sf::st_geometry(object))
  if("id" %in% names(object)) res$id <- object$id
  res$summary <- summarylist
  res <- sf::st_as_sf(tibble::as_tibble(res))
  return(res)
}
