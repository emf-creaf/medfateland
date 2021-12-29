.ggspl<-function(spl) {
  var = names(spl@data)
  if(inherits(spl, c("SpatialPixelsDataFrame", "SpatialGridDataFrame"))) {
    spl_sf =sf::st_as_sf(as(spl, "SpatialPolygonsDataFrame"))
    g<-ggplot()+
      geom_sf(spl_sf, mapping = aes_string(fill=var), col=NA)+
      theme_bw()
  } else {
    spl_sf =sf::st_as_sf(spl)
    g<-ggplot()+
      geom_sf(spl_sf, mapping = aes_string(col=var))+
      theme_bw()
  }
  return(g)
}
setMethod("plot", signature("SpatialPixelsLandscape"),
          function(x, y, ...) {
            return(.ggspl(getLandscapeLayer(x, y, ...)))
          })


setMethod("spplot", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })

setMethod("plot", signature("SpatialGridLandscape"),
          function(x, y, ...) {
            return(.ggspl(getLandscapeLayer(x, y, ...)))
          })


setMethod("spplot", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })


setMethod("plot", signature(x="SpatialPointsLandscape"),
          function(x, y, ...) {
            return(.ggspl(getLandscapeLayer(x, y, ...)))
          })

setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })

setMethod("spplot", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })

setMethod("plot", signature("DistributedWatershed"),
          function(x, y, ...) {
            return(.ggspl(getLandscapeLayer(x, y, ...)))
          })
