setMethod("spplot", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              spplot(getLandscapeVariable(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPixelsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(getLandscapeVariable(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialGridTopography"), variable, ...)
            }
          })


setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(getLandscapeVariable(obj, variable, ...))
            } else {
              spplot(as(obj, "SpatialPointsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            spplot(getLandscapeVariable(obj, variable, ...))
          })