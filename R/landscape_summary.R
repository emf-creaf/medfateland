
#' Forest and soil summaries over space
#' 
#' Functions to calculates a summary function for the forest or soil of all spatial elements 
#' in an object of class \code{\link[sf]{sf}} containing landscape information.
#' 
#' @param object An object of class \code{\link[sf]{sf}}.
#' @param name A string of the element to summarize: "forest", "soil" or "state".
#' @param summary_function A function that accepts objects of class \code{\link[medfate]{forest}}, \code{\link[medfate]{soil}} or model input objects, respectively.
#' @param ... Additional arguments to the summary function.
#' @param unlist Logical flag to try converting the summaries into different columns
#' @param progress Boolean flag to display progress information
#' 
#' @returns An object of class \code{\link[sf]{sf}} containing the calculated statistics. 
#' If \code{unlist = FALSE} column 'summary' is a list with summaries for each element. 
#' If \code{unlist = TRUE} different columns are returned instead, one per variable given in the summary function.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link[medfate]{forest}}, \code{\link[medfate]{soil}}, \code{\link[medfate]{summary.forest}}
#' 
#' @examples 
#' # Load plot data and species parameters from medfate
#' data(example_ifn)
#' 
#' # Load default medfate parameters
#' data("SpParamsMED")
#'  
#' # Apply forest summary function
#' landscape_summary(example_ifn, "forest", summary.forest, SpParamsMED)
#'  
#' @export
landscape_summary<-function(object, name, summary_function, ..., unlist = FALSE, progress = FALSE) {
  if(!inherits(object, "sf")) stop("'object' has to be an object of class 'sf'.")
  name = match.arg(name, c("forest", "soil", "state"))
  l = object[[name]]
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  sm = vector("list", length(l))
  if(progress) {
    cli::cli_li(paste0("Calculating summaries"))
    cli::cli_progress_bar(name = "Stands", total = length(l))
  }
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[[i]] = do.call(summary_function, args=list(object=l[[i]],...))
    if(progress) cli::cli_progress_update()
  }
  if(progress) {
    cli::cli_progress_done()
  }
  res = sf::st_sf(geometry=sf::st_geometry(object))
  if(!unlist) {
    res$summary = sm
  } else {
    if(progress) {
      cli::cli_li(paste0("Unlisting"))
      cli::cli_progress_bar(name = "Stands", total = length(l))
    }
    d1 = unlist(sm[[1]])
    nms = names(d1)
    for(nm in nms) res[[nm]] = NA
    for(i in 1:length(sm)) {
      if(!is.null(sm[[i]])) {
        res[i, nms] = unlist(sm[[i]])
      }
      if(progress) cli::cli_progress_update()
    }
    if(progress) {
      cli::cli_progress_done()
    }
  }
  return(sf::st_as_sf(tibble::as_tibble(res)))
}
