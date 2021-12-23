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
            if(y %in% .getAllowedVars()) {
              return(.ggspl(getLandscapeLayer(x, y, ...)))
            }
          })


setMethod("spplot", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              spplot(getLandscapeLayer(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPixelsTopography"), variable, ...)
            }
          })

setMethod("plot", signature("SpatialGridLandscape"),
          function(x, y, ...) {
            if(y %in% .getAllowedVars()) {
              return(.ggspl(getLandscapeLayer(x, y, ...)))
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


setMethod("plot", signature("SpatialPointsLandscape"),
          function(x, y, ...) {
            if(y %in% .getAllowedVars()) {
              return(.ggspl(getLandscapeLayer(x, y, ...)))
            }
          })

setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              spplot(getLandscapeLayer(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPointsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeLayer(obj, variable, ...))
          })

setMethod("plot", signature("DistributedWatershed"),
          function(x, y, ...) {
            return(.ggspl(getLandscapeLayer(x, y, ...)))
          })
