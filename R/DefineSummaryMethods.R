setGeneric("spatialSoilSummary", valueClass ="Spatial", function(object, summaryFunction, ...){
  standardGeneric("spatialSoilSummary")
})
setMethod("spatialSoilSummary", signature("SpatialPointsLandscape"), function(object, summaryFunction, ...) {
  l = object@soillist
  if(length(l)==0) return(NULL)
  firstNoNa = which(!unlist(lapply(l,is.na)))[1]
  s = do.call(summaryFunction, args=list(object=l[[firstNoNa]],...))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!is.na(l[[i]])) sm[i,] = do.call(summaryFunction, args=list(object=l[[i]],...))
  }
  rownames(sm) = rownames(object@coords)
  s = sm
  return(SpatialPointsDataFrame(coords=object@coords, data = s, 
                                proj4string=object@proj4string, 
                                bbox = object@bbox))
})

setMethod("spatialSoilSummary", signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
  l = object@soillist
  if(length(l)==0) return(NULL)
  firstNoNa = which(!unlist(lapply(l,is.na)))[1]
  s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!is.na(l[i])) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
  }
  rownames(sm) = rownames(coordinates(object))
  s = sm
  return(SpatialGridDataFrame(grid = object@grid, data = s,
                              proj4string=object@proj4string))
})
setMethod("spatialSoilSummary", signature("SpatialPixelsLandscape"), function(object, summaryFunction, ...) {
  l = object@soillist
  if(length(l)==0) return(NULL)
  firstNoNa = which(!unlist(lapply(l,is.na)))[1]
  s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
  sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
  colnames(sm) = names(s)
  for(i in 1:length(l)) {
    if(!is.na(l[i])) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
  }
  rownames(sm) = rownames(coordinates(object))
  s = sm
  return(SpatialPixelsDataFrame(grid = object@grid, data = s,
                                proj4string=object@proj4string))
})

#### spatialForestSummary ####
setGeneric("spatialForestSummary", valueClass ="Spatial",
           function(object, summaryFunction, ...){
             standardGeneric("spatialForestSummary")
           })

setMethod("spatialForestSummary", signature("SpatialPointsLandscape"), 
          function(object, summaryFunction, ...) {
            l = object@forestlist
            if(length(l)==0) return(NULL)
            firstNoNa = which(!unlist(lapply(l,is.na)))[1]
            s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
            sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
            colnames(sm) = names(s)
            for(i in 1:length(l)) {
              if(!is.na(l[i])) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
            }
            rownames(sm) = rownames(object@coords)
            s = sm
            return(SpatialPointsDataFrame(coords=object@coords, data = s, 
                                          proj4string=object@proj4string, 
                                          bbox = object@bbox))
          })


setMethod("spatialForestSummary",
          signature("SpatialGridLandscape"), function(object, summaryFunction, ...) {
            l = object@forestlist
            if(length(l)==0) return(NULL)
            firstNoNa = which(!unlist(lapply(l,is.na)))[1]
            s = unlist(do.call(summaryFunction, args=list(object=l[[firstNoNa]],...)))
            sm = data.frame(matrix(NA, nrow=length(l), ncol=length(s)))
            colnames(sm) = names(s)
            for(i in 1:length(l)) {
              if(!is.na(l[i])) sm[i,] = unlist(do.call(summaryFunction, args=list(object=l[[i]],...)))
            }
            rownames(sm) = rownames(coordinates(object))
            s = sm
            return(SpatialGridDataFrame(grid = object@grid, data = s,
                                        proj4string=object@proj4string))
          })
