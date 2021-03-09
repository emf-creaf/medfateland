.modelspatial<-function(y, SpParams, meteo, model = "spwb",
                        control = defaultControl(), dates = NULL,
                        mergeTrees = FALSE,
                        summaryFunction=NULL, args=NULL,
                        parallelize = FALSE, progress = TRUE) {
  sp = as(y,"SpatialPoints")
  topo = y@data
  spt = SpatialPointsTopography(sp, topo$elevation, topo$slope, topo$aspect)
  longlat = spTransform(sp,CRS("+proj=longlat"))
  latitude = longlat@coords[,2]
  elevation = y@data$elevation
  slope = y@data$slope
  aspect = y@data$aspect

  control$verbose = FALSE

  forestlist = y@forestlist
  soillist  = y@soillist
  xlist  = y@xlist


  n = length(forestlist)
  reslist = vector("list",n)

  initf<-function(i) {
    f = forestlist[[i]]
    s = soillist[[i]]
    x = NULL
    if((!is.null(f)) && (!is.null(s))) {
      if(mergeTrees) f = forest_mergeTrees(f)
      if(model=="spwb") {
        x = medfate::forest2spwbInput(f, s, SpParams, control)
      } else if(model=="growth") {
        x = medfate::forest2growthInput(f, s, SpParams, control)
      }
    }
    return(x)
  }

  if(progress) cat("Initializing:\n")
  if(progress) pb = txtProgressBar(0, n, style=3)
  for(i in 1:n) {
    if(progress) setTxtProgressBar(pb, i)
    xlist[[i]] = initf(i)
  }
  if(progress) cat("\n")

  simf<-function(i, sfun = NULL){
    f = forestlist[[i]]
    s = soillist[[i]]
    x = xlist[[i]]
    if((!is.null(f)) && (!is.null(s))) {
      if(inherits(meteo,"data.frame")) met = meteo
      else if(inherits(meteo,"SpatialPointsMeteorology") || inherits(meteo,"SpatialGridMeteorology")|| inherits(meteo,"SpatialPixelsMeteorology")) {
        met = meteo@data[[i]]
      } else if(inherits(meteo, "MeteorologyInterpolationData")) {
        met = meteoland::interpolationpoints(meteo, spt[i], dates=dates, verbose=FALSE)
        met = met@data[[1]]
      }
      if(!is.null(dates)) met = met[as.character(dates),] #subset dates
      if(model=="spwb") {
        S<-medfate::spwb(x, meteo=met,
                         latitude = latitude[i], elevation = elevation[i],
                         slope = slope[i], aspect = aspect[i])
      } else if(model=="growth") {
        S<-medfate::growth(x, meteo=met,
                           latitude = latitude[i], elevation = elevation[i],
                           slope = slope[i], aspect = aspect[i])
      }
      if(!is.null(sfun)){
        argList = list(object=S)
        if(!is.null(args)) {
          for(j in 1:length(args)) argList[names(args)[j]]=args[j]
        }
        res = do.call(sfun, args=argList)
      } else {
        res = S
      }
      return(res)
    }
    return(NULL)
  }

  if(parallelize) {
    if(progress) cat("Simulation...")
    env<-environment()
    cl<-makeCluster(detectCores()-1)
    varlist = c("forestlist", "soillist", "mergeTrees", "meteo", "model",
                "SpParams", "control", "latitude", "elevation", "slope", "aspect",
                "dates", "args", "xlist")
    clusterExport(cl, varlist, envir = env)
    reslist = clusterApply(cl, 1:n, simf, sfun = summaryFunction)
    stopCluster(cl)
    if(progress) cat("done.\n")
  } else {
    if(progress) cat("Simulation:\n")
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      reslist[[i]] = simf(i, sfun = summaryFunction)
    }
  }

  return(list(xlist = xlist, reslist = reslist))
}


spwbpoints<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialPointsLandscape"))
    stop("'y' has to be of class 'SpatialPointsLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialPointsMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialPointsMeteorology' or 'MeteorologyInterpolationData'.")
  if(inherits(meteo,"SpatialPointsMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("spwbpoints","list")
  return(res)
}
spwbgrid<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                   summaryFunction=NULL, args=NULL,
                   parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialGridLandscape"))
    stop("'y' has to be of class 'SpatialGridLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialGridMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialGridMeteorology' or 'MeteorologyInterpolationData'.")
  if(inherits(meteo,"SpatialGridMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(grid = y@grid, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("spwbgrid","list")
  return(res)
}
spwbpixels<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialPixelsLandscape"))
    stop("'y' has to be of class 'SpatialPixelsLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialPixelsMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialPixelsMeteorology' or 'MeteorologyInterpolationData'.")

  #Get spatial object properties
  if(inherits(meteo,"SpatialPixelsMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, coords.nrs = y@coords.nrs, grid = y@grid, grid.index = y@grid.index, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("spwbpixels","list")
  return(res)
}
growthpoints<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialPointsLandscape"))
    stop("'y' has to be of class 'SpatialPointsLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialPointsMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialPointsMeteorology' or 'MeteorologyInterpolationData'.")
  if(inherits(meteo,"SpatialPointsMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("growthpoints","list")
  return(res)
}
growthgrid<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialGridLandscape"))
    stop("'y' has to be of class 'SpatialGridLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialGridMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialGridMeteorology' or 'MeteorologyInterpolationData'.")
  if(inherits(meteo,"SpatialGridMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(grid = y@grid, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("growthgrid","list")
  return(res)
}
growthpixels<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {

  #Check input
  if(!inherits(y,"SpatialPixelsLandscape"))
    stop("'y' has to be of class 'SpatialPixelsLandscape'.")
  if(!inherits(meteo,"data.frame") &&
     !inherits(meteo,"SpatialPixelsMeteorology") &&
     !inherits(meteo,"MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'data.frame', 'SpatialPixelsMeteorology' or 'MeteorologyInterpolationData'.")

  #Get spatial object properties
  if(inherits(meteo,"SpatialPixelsMeteorology")) {
    ycoords = coordinates(y)
    mcoords = coordinates(meteo)
    if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  }

  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", control = control, dates = dates,
                    summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, coords.nrs = y@coords.nrs, grid = y@grid, grid.index = y@grid.index, bbox = y@bbox, proj4string = y@proj4string, xlist = l$xlist, reslist = l$reslist)
  class(res) = c("growthpixels","list")
  return(res)
}
