.getAllowedVars <-function() {
  return(c("lct", "basalArea", "SWE", "WTD","texture1", "texture2", "texture3",
           "SoilVol"))
}
.getPlotVar<-function(obj, variable, ...) {
  variable = match.arg(variable, .getAllowedVars())
  if(variable=="lct") {
    varplot = factor(obj@lct)
  } else if(variable=="basalArea") {
    n = length(obj@forestlist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      f = obj@forestlist[[i]]
      if(!is.null(f)) varplot[i] = sum(plant_basalArea(f), na.rm=T)
    }
  } else if(variable=="SWE") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = s[["SWE"]]
    }
    varplot = factor(varplot)
  } else if(variable=="WTD") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = soil_waterTableDepth(s, ...)
    }
    varplot = factor(varplot)
  } else if(variable=="texture1") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = soil_USDAType(s$clay[1],s$sand[1])
    }
    varplot = factor(varplot)
  } else if(variable=="texture2") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = soil_USDAType(s$clay[2],s$sand[2])
    }
    varplot = factor(varplot)
  } else if(variable=="texture3") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = soil_USDAType(s$clay[3],s$sand[3])
    }
    varplot = factor(varplot)
  } else if(variable=="SoilVol") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(!is.null(s)) varplot[i] = sum(soil_water(s, ...), na.rm=T)
    }
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

setMethod("spplot", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            if(variable %in% c("numNeigh", "waterOrder")) {
              if(variable=="numNeigh") {
                varplot = sapply(obj@queenNeigh,"length")
              } else if(variable=="waterOrder") {
                varplot = rank(-obj@data$elevation, ties.method="first")
              }
              spplot(SpatialPixelsDataFrame(as(obj,"SpatialPoints"),
                                            data.frame(var = varplot), 
                                            grid = obj@grid),
                     ...)
              
            } else {
              spplot(as(obj, "SpatialPixelsLandscape"), variable, ...)
            }
          })