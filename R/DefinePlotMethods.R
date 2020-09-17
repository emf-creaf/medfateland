.getAllowedVars <-function() {
  return(c("lct", "basalArea", "texture1"))
}
.getPlotVar<-function(obj, variable, ...) {
  if(variable=="lct") {
    varplot = factor(obj@lct)
  } else if(variable=="basalArea") {
    n = length(obj@forestlist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      f = obj@forestlist[[i]]
      if(!is.null(f)) varplot[i] = sum(plant_basalArea(f), na.rm=T)
    }
  } else if(variable=="texture1") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = soil_USDAType(s$clay[1],s$sand[1])
    }
    varplot = factor(varplot)
  } else if(variable=="W1") {
    n = length(obj@soillist)
    W1 = rep(NA, n)
    for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W1[i] = obj@soillist[[i]]$W[1]
    spplot(SpatialGridDataFrame(obj@grid, data.frame(W1 = W1)), at = seq(0,1, by=0.01),...)
  } else if(variable=="W2") {
    n = length(obj@soillist)
    W2 = rep(NA, n)
    for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W2[i] = obj@soillist[[i]]$W[2]
    spplot(SpatialGridDataFrame(obj@grid, data.frame(W2 = W2)), at = seq(0,1, by=0.01),...)
  } else if(variable=="W3") {
    n = length(obj@soillist)
    W3 = rep(NA, n)
    for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W3[i] = obj@soillist[[i]]$W[3]
    spplot(SpatialGridDataFrame(obj@grid, data.frame(W3 = W3)), at = seq(0,1, by=0.01),...)
  } else if(variable=="WTD") {
    n = length(obj@soillist)
    WTD = rep(NA, n)
    for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) WTD[i] = medfate::soil_waterTableDepth(obj@soillist[[i]])
    spplot(SpatialGridDataFrame(obj@grid, data.frame(WTD = WTD)), ...)
  } 
  return(varplot)
}
setMethod("spplot", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              spplot(SpatialPixelsDataFrame(as(obj,"SpatialPoints"),
                                            data.frame(var = .getPlotVar(obj, variable, ...)), 
                                            grid = obj@grid),
                     ...)
              
            } else {
              spplot(as(obj, "SpatialPixelsTopography"), variable, ...)
            }
          })

setMethod("spplot", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(SpatialGridDataFrame(obj@grid,
                                          data.frame(var = ,getPlotVar(obj, variable, ...)), 
                                          proj4string = obj@proj4string),
                     ...)
              
            } else {
              spplot(as(obj, "SpatialGridTopography"), variable, ...)
            }
          })


setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              spplot(SpatialPointsDataFrame(obj@coords,
                                            data.frame(var = ,getPlotVar(obj, variable, ...)), 
                                            proj4string = obj@proj4string),
                     ...)
              
            } else {
              spplot(as(obj, "SpatialPointsTopography"), variable, ...)
            }
          })
