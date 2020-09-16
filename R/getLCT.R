setGeneric("getLCTs", valueClass ="Spatial", function(object){
  standardGeneric("getLCTs")
})
setMethod("getLCTs", signature("SpatialPointsLandscape"), function(object) {
  return(SpatialPointsDataFrame(points = as(object, "SpatialPoints"),
                                data=data.frame(LCT=object@lct)))
})
setMethod("getLCTs", signature("SpatialPixelsLandscape"), function(object) {
  return(SpatialPixelsDataFrame(points = SpatialPoints(object@coords, object@proj4string),
                                grid=object@grid,
                                data=data.frame(LCT=object@lct)))
})
setMethod("getLCTs", signature("SpatialGridLandscape"), function(object) {
  return(SpatialGridDataFrame(grid=object@grid,
                              data=data.frame(LCT=object@lct),
                              proj4string=object@proj4string))
})
