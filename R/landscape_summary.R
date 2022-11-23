
#' Forest and soil summaries over space
#' 
#' Functions to calculates a summary function for the forest or soil of all spatial elements 
#' in an object of class \code{\link{sf}} containing landscape information.
#' 
#' @param object An object of class \code{\link{sf}}.
#' @param name A string of the element to summarize: "forest", "soil" or "state".
#' @param summary_function A function that accepts objects of class \code{\link{forest}}, \code{soil} or model input objects, respectively.
#' @param ... Additional arguments to the summary function.
#' @param unlist Logical flag to try converting the summaries into different columns
#' 
#' @returns An object of class \code{\link{sf}} containing the calculated statistics. 
#' If \code{unlist = FALSE} column 'summary' is a list with summaries for each element. 
#' If \code{unlist = TRUE} different columns are returned instead, one per variable given in the summary function.
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
landscape_summary<-function(object, name, summary_function, ..., unlist = FALSE) {
  if(!inherits(object, "sf")) stop("'object' has to be an object of class 'sf'.")
  name = match.arg(name, c("forest", "soil", "state"))
  l = object[[name]]
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  sm = vector("list", length(l))
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[[i]] = do.call(summary_function, args=list(object=l[[i]],...))
  }
  res = sf::st_sf(geometry=sf::st_geometry(object))
  if(!unlist) {
    res$summary = sm
  } else {
    d1 = unlist(sm[[1]])
    nms = names(d1)
    for(nm in nms) res[[nm]] = NA
    for(i in 1:length(sm)) {
      res[i, nms] = unlist(sm[[i]])
    }
  }
  return(sf::st_as_sf(tibble::as_tibble(res)))
}
