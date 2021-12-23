.getAllowedVars <-function() {
  return(c("elevation", "slope", "aspect", "lct", "basalArea", "SWE", "WTD","texture1", "texture2", "texture3",
           "SoilVol"))
}
.getLandscapeVar<-function(obj, variable, ...) {
  n = length(obj@forestlist)
  varplot = rep(NA, n)
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
    for(i in 1:n) {
      f = obj@forestlist[[i]]
      if(inherits(f,"forest")) varplot[i] = stand_basalArea(f)
    }
  } else if(variable=="SWE") {
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(inherits(s,"soil")) varplot[i] = s[["SWE"]]
    }
  } else if(variable=="WTD") {
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(inherits(s,"soil")) varplot[i] = soil_waterTableDepth(s, ...)
    }
  } else if(variable=="texture1") {
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(inherits(s,"soil")) varplot[i] = soil_USDAType(s$clay[1],s$sand[1])
    }
    varplot = factor(varplot)
  } else if(variable=="texture2") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(inherits(s,"soil")) varplot[i] = soil_USDAType(s$clay[2],s$sand[2])
    }
    varplot = factor(varplot)
  } else if(variable=="texture3") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(class(s)[1]=="soil") varplot[i] = soil_USDAType(s$clay[3],s$sand[3])
    }
    varplot = factor(varplot)
  } else if(variable=="SoilVol") {
    n = length(obj@soillist)
    varplot = rep(NA, n)
    for(i in 1:n) {
      s = obj@soillist[[i]]
      if(class(s)[1]=="soil") varplot[i] = sum(soil_water(s, ...), na.rm=T)
    }
  } 
  return(varplot)
}

setGeneric("getLandscapeVariable", function(obj, variable = "lct", ...){
  standardGeneric("getLandscapeVariable")
})
setMethod("getLandscapeVariable", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              return(.getLandscapeVar(obj, variable, ...))
            } 
          })

setMethod("getLandscapeVariable", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              return(.getLandscapeVar(obj, variable, ...))
            } 
          })
setMethod("getLandscapeVariable", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              return(.getLandscapeVar(obj, variable, ...))
            } 
          })

setMethod("getLandscapeVariable", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            if(variable %in% c("numNeigh", "waterOrder", "outlets", "channel","DTB","RockPorosity", "RockConductivity",
                               "AquiferElevation", "DTA","AquiferVolume", "SWE")) {
              if(variable=="numNeigh") {
                varplot = sapply(obj@queenNeigh,"length")
              } else if(variable=="waterOrder") {
                wo = obj@waterOrder
                varplot = 1:length(wo)
                varplot[wo] = 1:length(wo)
              } else if(variable=="outlets") {
                outlets = which(unlist(lapply(obj@waterQ, sum))==0)
                varplot = rep(FALSE, length(obj@waterQ))
                varplot[outlets] = TRUE
              } else if(variable=="channel") {
                varplot = obj@channel
              } else if(variable =="DTB") {
                varplot = obj@bedrock$DepthToBedrock/1000.0  # in m
              } else if(variable =="RockPorosity") {
                varplot = obj@bedrock$Porosity
              } else if(variable =="RockConductivity") {
                varplot = obj@bedrock$Conductivity
              } else if(variable =="AquiferVolume") {
                varplot = obj@aquifer
              } else if(variable =="AquiferElevation") {
                DTB = obj@bedrock$DepthToBedrock
                aquifer = obj@aquifer
                RockPorosity = obj@bedrock$Porosity
                elevation = obj@data$elevation
                varplot = elevation - (DTB/1000.0) + (aquifer/RockPorosity)/1000.0 # in m
                varplot[RockPorosity==0.0] = elevation[RockPorosity==0.0]
              } else if(variable=="SWE") {
                varplot = obj@snowpack
              } else if(variable =="DTA") {
                DTB = obj@bedrock$DepthToBedrock
                aquifer = obj@aquifer
                RockPorosity = obj@bedrock$Porosity
                varplot = (DTB/1000.0) - (aquifer/RockPorosity)/1000.0
                varplot[RockPorosity==0.0] = DTB[RockPorosity==0.0]/1000
              }
              return(varplot)
            } else {
              return(getLandscapeVariable(as(obj, "SpatialPixelsLandscape"), variable, ...))
            } 
          })


setGeneric("getLandscapeLayer", valueClass ="Spatial", function(obj, variable = "lct", ...){
  standardGeneric("getLandscapeLayer")
})
setMethod("getLandscapeLayer", signature("SpatialPixelsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              df<-data.frame(y = .getLandscapeVar(obj, variable, ...))
              names(df) <- variable
              return(SpatialPixelsDataFrame(as(obj,"SpatialPoints"), df, grid = obj@grid))
            } 
          })
setMethod("getLandscapeLayer", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              df<-data.frame(y = .getLandscapeVar(obj, variable, ...))
              names(df) <- variable
              return(SpatialGridDataFrame(obj@grid, df, proj4string = obj@proj4string))
            } 
          })
setMethod("getLandscapeLayer", signature("SpatialPointsLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable %in% .getAllowedVars()) {
              df<-data.frame(y = .getLandscapeVar(obj, variable, ...))
              names(df) <- variable
              return(SpatialPointsDataFrame(obj@coords, df,proj4string = obj@proj4string))
              
            } 
          })
setMethod("getLandscapeLayer", signature("DistributedWatershed"),
          function(obj, variable = "lct", ...) {
            return(SpatialPixelsDataFrame(as(obj,"SpatialPoints"),
                                          data.frame(var = getLandscapeVariable(obj, variable, ...)), 
                                          grid = obj@grid))
          })


