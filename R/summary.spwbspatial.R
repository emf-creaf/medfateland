.summary_sp<-function(object, summaryFunction, ...) {
  n = length(object$resultlist)
  summarylist = vector("list", n)
  names(summarylist) = names(object$resultlist)
  for(i in 1:n) {
    if(!is.null(object$resultlist[[i]])) {
      argList = list(object=object$resultlist[[i]],...)
      summarylist[[i]] = do.call(summaryFunction, args=argList)
    }
  }
  res = list(sp = object$sp, summarylist = summarylist)
  class(res) <- c("summaryspatial", "list")
  return(res)
}

#' Summarizes spatial simulation results
#' 
#' Creates spatial objects containing summaries of simulations
#' 
#' @param object An object of the appropriate class, containing simulation results.
#' @param summaryFunction The summary function to be executed on simulation results (see details).
#' @param ... Additional parameters to the summary function.
#' 
#' @details The function supplied should take as input an object of local simulation function, i.e. \code{\link{spwb}}, \code{\link{growth}}, or \code{\link{fordyn}}. 
#' The output should be a matrix with dates as rows and variables in columns. 
#' An example of suitable function is \code{\link{summary.spwb}}.
#' 
#' @return An object of class \code{\link{summaryspatial}}, with the following two elements:
#'  \itemize{
#'    \item{\code{sp}: An object with spatial information (of \code{SpatialPoints-class}, \code{SpatialPixels-class} or \code{SpatialGrid-class}).}
#'    \item{\code{summarylist}: A list of model output summaries for each simulated stand.}
#'  }
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwbspatial}}, \code{\link{plot.summaryspatial}}
#' 
#' @name summary.spwbspatial
summary.spwbspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
#' @rdname summary.spwbspatial
summary.growthspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
#' @rdname summary.spwbspatial
summary.fordynspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}