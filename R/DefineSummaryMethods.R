
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

setGeneric("spatialSoilSummary", valueClass ="Spatial", function(object, summaryFunction, ...){
  standardGeneric("spatialSoilSummary")
})
setMethod("spatialSoilSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "soillist", summaryFunction, ...))
})
setMethod("spatialSoilSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "soillist", summaryFunction, ...))
})
setMethod("spatialSoilSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "soillist", summaryFunction, ...))
})


#### spatialForestSummary ####
setGeneric("spatialForestSummary", valueClass ="Spatial",
           function(object, summaryFunction, ...){
             standardGeneric("spatialForestSummary")
           })
setMethod("spatialForestSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "forestlist", summaryFunction, ...))
})
setMethod("spatialForestSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "forestlist", summaryFunction, ...))
})
setMethod("spatialForestSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "forestlist", summaryFunction, ...))
})



#### spatialModelInputSummary ####
setGeneric("spatialModelInputSummary", valueClass ="Spatial",
           function(object, summaryFunction, ...){
             standardGeneric("spatialModelInputSummary")
           })
setMethod("spatialModelInputSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPointsSummary(object, "xlist", summaryFunction, ...))
})
setMethod("spatialModelInputSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  return(.spatialPixelsSummary(object, "xlist", summaryFunction, ...))
})
setMethod("spatialModelInputSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  return(.spatialGridSummary(object, "xlist", summaryFunction, ...))
})