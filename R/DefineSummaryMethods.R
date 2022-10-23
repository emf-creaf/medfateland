
.spatialPointsSummary<-function(object, name, summaryFunction, ...) {
  l = slot(object, name)
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  firstNoNa = which(!l_isnull)[1]
  s = do.call(summaryFunction, args=list(object=l[[firstNoNa]],...))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[i,] = do.call(summaryFunction, args=list(object=l[[i]],...))
  }
  rownames(sm) = rownames(object@coords)
  s = sm
  return(SpatialPointsDataFrame(coords=object@coords, data = s, 
                                proj4string=object@proj4string, 
                                bbox = object@bbox))
}
.spatialPixelsSummary<-function(object, name, summaryFunction, ...) {
  l = slot(object, name)
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  firstNoNa = which(!l_isnull)[1]
  s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
  }
  rownames(sm) = rownames(coordinates(object))
  s = sm
  return(SpatialPixelsDataFrame(as(object, "SpatialPoints"),
                                data = s,
                                grid = object@grid))
}
.spatialGridSummary<-function(object, name, summaryFunction, ...) {
  l = slot(object, name)
  if(length(l)==0) return(NULL)
  l_isnull = unlist(lapply(l,is.null))
  firstNoNa = which(!l_isnull)[1]
  s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!l_isnull[i]) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
  }
  rownames(sm) = rownames(coordinates(object))
  s = sm
  return(SpatialGridDataFrame(grid = object@grid, data = s,
                              proj4string=object@proj4string))
}


setGeneric("spatialForestSummary", valueClass ="Spatial",
           function(object, summaryFunction, ...){
             standardGeneric("spatialForestSummary")
           })
setGeneric("spatialSoilSummary", valueClass ="Spatial", function(object, summaryFunction, ...){
  standardGeneric("spatialSoilSummary")
})
setGeneric("spatialModelInputSummary", valueClass ="Spatial",
           function(object, summaryFunction, ...){
             standardGeneric("spatialModelInputSummary")
           })

#' Forest and soil summaries over space
#' 
#' Functions to calculates a summary function for the forest or soil of all spatial elements in an object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.
#' 
#' @param object An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.
#' @param summaryFunction A function that accepts objects of class \code{\link{forest}} or \code{soil}, respectively.
#' @param ... Additional arguments to the summary function.
#' 
#' @returns An object of class \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialPixelsDataFrame}} or \code{\link{SpatialGridDataFrame}}, depending on the input, containing the calculated statistics. 
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}
#' 
#' @examples 
#'  library(medfate)
#'  # Load plot data and species parameters from medfate
#'  data(examplepointslandscape)
#'  data(SpParamsMED)
#' 
#'  # Apply summary function
#'  y <- spatialForestSummary(examplepointslandscape,summary.forest, SpParamsMED)
#'  head(y@data)
#'  # Plot basal area
#'  spplot(y["BA"])
#'  
#' @name spatialForestSummary
setMethod("spatialForestSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "forestlist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialForestSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "forestlist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialForestSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "forestlist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialSoilSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "soillist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialSoilSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "soillist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialSoilSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "soillist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialModelInputSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "xlist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialModelInputSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "xlist", summaryFunction, ...))
})
#' @name spatialForestSummary
setMethod("spatialModelInputSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "xlist", summaryFunction, ...))
})