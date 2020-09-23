# .ggspl<-function(spl) {
#   spl_sf =st_as_sf(as(spl, "SpatialPolygonsDataFrame"))
#   g<-ggplot()+
#     geom_sf(spl_sf, mapping = aes(fill=var), col=NA)
#   return(g)
# }
# setMethod("plot", signature("SpatialPixelsLandscape"),
#           function(x, variable = "lct", ...) {
#             if(variable %in% .getAllowedVars()) {
#               return(.ggspl(getLandscapeVariable(x, variable, ...)))
#             }
#           })

setMethod("spplot", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              spplot(getLandscapeLayer(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPixelsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(getLandscapeLayer(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialGridTopography"), variable, ...)
            }
          })


setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(getLandscapeLayer(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPointsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })