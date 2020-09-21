.getAllowedVars <-function() {
  return(c("elevation", "slope", "aspect", "lct", "basalArea", "SWE", "WTD","texture1", "texture2", "texture3",
           "SoilVol"))
}
.getLandscapeVar<-function(obj, variable, ...) {
  variable = match.arg(variable, .getAllowedVars())
  if(variable=="lct") {
    varplot = factor(obj@lct)
  } else if(variable=="elevation") {
    varplot = obj@data$elevation
  } else if(variable=="slope") {
    varplot = obj@data$slope
  } else if(variable=="aspect") {
    varplot = obj@data$aspect
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
setGeneric("getLandscapeVariable", valueClass ="Spatial", function(obj, variable = "lct", ...){
  standardGeneric("getLandscapeVariable")
})
setMethod("getLandscapeVariable", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              return(SpatialPixelsDataFrame(as(obj,"SpatialPoints"),
                                            data.frame(var = .getLandscapeVar(obj, variable, ...)), 
                                            grid = obj@grid))
              
            } 
          })

setMethod("getLandscapeVariable", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              return(SpatialGridDataFrame(obj@grid,
                                          data.frame(var = .getLandscapeVar(obj, variable, ...)), 
                                          proj4string = obj@proj4string))
              
            } 
          })


setMethod("getLandscapeVariable", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(var %in% .getAllowedVars()) {
              return(SpatialPointsDataFrame(obj@coords,
                                            data.frame(var = .getLandscapeVar(obj, variable, ...)), 
                                            proj4string = obj@proj4string))
              
            } 
          })

setMethod("getLandscapeVariable", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            if(variable %in% c("numNeigh", "waterOrder", "DTB","RockPorosity", "RockConductivity",
                               "AquiferElevation")) {
              if(variable=="numNeigh") {
                varplot = sapply(obj@queenNeigh,"length")
              } else if(variable=="waterOrder") {
                wo = dw@waterOrder
                varplot = 1:length(wo)
                varplot[wo] = 1:length(wo)
              } else if(variable =="DTB") {
                varplot = obj@bedrock$DepthToBedrock
              } else if(variable =="RockPorosity") {
                varplot = obj@bedrock$Porosity
              } else if(variable =="RockConductivity") {
                varplot = obj@bedrock$Conductivity
              } else if(variable =="AquiferElevation") {
                DTB = obj@bedrock$DepthToBedrock
                aquifer = obj@aquifer
                RockPorosity = obj@bedrock$Porosity
                elevation = obj@data$elevation
                varplot = elevation - (DTB/1000.0) + (aquifer/RockPorosity)/1000.0;
              }
              return(SpatialPixelsDataFrame(as(obj,"SpatialPoints"),
                                            data.frame(var = varplot), 
                                            grid = obj@grid))
              
            } else {
              return(getLandscapeVariable(as(obj, "SpatialPixelsLandscape"), variable, ...))
            } 
          })
