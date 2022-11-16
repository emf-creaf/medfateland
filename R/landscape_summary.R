
#' Forest and soil summaries over space
#' 
#' Functions to calculates a summary function for the forest or soil of all spatial elements 
#' in an object of class \code{\link{sf}} containing landscape information.
#' 
#' @param object An object of class \code{\link{sf}}.
#' @param name A string of the element to summarize: "forest", "soil" or "state".
#' @param summaryFunction A function that accepts objects of class \code{\link{forest}}, \code{soil} or model input objects, respectively.
#' @param ... Additional arguments to the summary function.
#' 
#' @returns An object of class \code{\link{sf}} containing the calculated statistics. 
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}
#' 
#' @examples 
#' # Load plot data and species parameters from medfate
#' data(examplepointslandscape)
#' 
#' # Load default medfate parameters
#' data("SpParamsMED")
#'  
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplepointslandscape)
#' 
#' # Apply forest summary function
#' landscape_summary(y, "forest", summary.forest, SpParamsMED)
#'  
landscape_summary<-function(object, name, summaryFunction, ...) {
  if(!inherits(object, "sf")) stop("'object' has to be an object of class 'sf'.")
  name = match.arg(name, c("forest", "soil", "state"))
  l = object[[name]]
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  sm = vector("list", length(l))
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[[i]] = do.call(summaryFunction, args=list(object=l[[i]],...))
  }
  res = sf::st_sf(geometry=sf::st_geometry(y))
  res$summary = sm
  return(sf::st_as_sf(tibble::as_tibble(res)))
}
