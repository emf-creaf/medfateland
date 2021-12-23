.modelspatial<-function(y, SpParams, meteo, model = "spwb",
                        control = defaultControl(), dates = NULL,
                        mergeTrees = FALSE, keepResults = TRUE,
                        summaryFunction=NULL, args=NULL,
                        parallelize = FALSE, progress = TRUE) {
  sp = as(y,"SpatialPoints")
  topo = y@data
  spt = SpatialPointsTopography(sp, topo$elevation, topo$slope, topo$aspect)
  longlat = spTransform(sp,CRS(SRS_string = "EPSG:4326"))
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
  summarylist = vector("list",n)
  names(reslist) = names(forestlist)
  names(summarylist) = names(forestlist)
  
  initf<-function(i) {
    f = forestlist[[i]]
    s = soillist[[i]]
    x = NULL
    if(inherits(f, "forest") && inherits(s, "soil")) {
      if(mergeTrees) f = forest_mergeTrees(f)
      if(model=="spwb") {
        x = medfate::forest2spwbInput(f, s, SpParams, control)
      } else if(model=="growth") {
        x = medfate::forest2growthInput(f, s, SpParams, control)
      }
    }
    return(x)
  }

  if(model %in% c("spwb", "growth")) {
    if(progress) cat("Initializing:\n")
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      xlist[[i]] = initf(i)
    }
    if(progress) cat("\n")
  }

  simf<-function(i, sfun = NULL){
    f = forestlist[[i]]
    s = soillist[[i]]
    x = xlist[[i]]
    res = NULL
    if(inherits(meteo,"data.frame")) met = meteo
    else if(inherits(meteo,"SpatialPointsMeteorology") || inherits(meteo,"SpatialGridMeteorology")|| inherits(meteo,"SpatialPixelsMeteorology")) {
      met = meteo@data[[i]]
    } else if(inherits(meteo, "MeteorologyInterpolationData")) {
      met = meteoland::interpolationpoints(meteo, spt[i], dates=dates, verbose=FALSE)
      met = met@data[[1]]
    }
    if(!is.null(dates)) met = met[as.character(dates),] #subset dates
    if(model=="spwb") {
      if(inherits(x, "spwbInput")){
        res<-medfate::spwb(x, meteo=met,
                           latitude = latitude[i], elevation = elevation[i],
                           slope = slope[i], aspect = aspect[i])
      } 
    } else if(model=="growth") {
      if(inherits(x, "growthInput")) {
        res<-medfate::growth(x, meteo=met,
                             latitude = latitude[i], elevation = elevation[i],
                             slope = slope[i], aspect = aspect[i])
      } 
    } else if(model=="fordyn") {
      if(inherits(f, "forest") && inherits(s, "soil")) {
        res<-medfate::fordyn(forest = f, soil = s, SpParams = SpParams, meteo=met, control = control,
                             latitude = latitude[i], elevation = elevation[i],
                             slope = slope[i], aspect = aspect[i])
      }
    } 
    if(!is.null(sfun) && !is.null(res)){
      argList = list(object=res)
      if(!is.null(args)) {
        for(j in 1:length(args)) argList[names(args)[j]]=args[j]
      }
      s = do.call(sfun, args=argList)
    } else {
      s = NULL
    }
    return(list(result = res, summary = s))
  }

  if(parallelize) {
    if(progress) cat("Simulation...")
    env<-environment()
    cl<-parallel::makeCluster(parallel::detectCores()-1)
    varlist = c("forestlist", "soillist", "mergeTrees", "meteo", "model",
                "SpParams", "control", "latitude", "elevation", "slope", "aspect",
                "dates", "args", "xlist")
    parallel::clusterExport(cl, varlist, envir = env)
    reslist_parallel = parallel::clusterApply(cl, 1:n, simf, sfun = summaryFunction)
    parallel::stopCluster(cl)
    for(i in 1:n) {
      reslist[[i]] = reslist_parallel[[i]]$result
      summarylist[[i]] = reslist_parallel[[i]]$summary
    }
    if(progress) cat("done.\n")
  } else {
    if(progress) cat("Simulation:\n")
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      sim_out = simf(i, sfun = summaryFunction)
      if(keepResults) reslist[[i]] = sim_out$result
      summarylist[[i]] = sim_out$summary
    }
  }

  return(list(xlist = xlist, resultlist = reslist, summarylist = summarylist))
}


spwbpoints<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("spwbpoints","list")
  return(res)
}
spwbgrid<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                   keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(grid = y@grid, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("spwbgrid","list")
  return(res)
}
spwbpixels<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, coords.nrs = y@coords.nrs, grid = y@grid, grid.index = y@grid.index, 
             bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("spwbpixels","list")
  return(res)
}
growthpoints<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("growthpoints","list")
  return(res)
}
growthgrid<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(grid = y@grid, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("growthgrid","list")
  return(res)
}
growthpixels<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, coords.nrs = y@coords.nrs, grid = y@grid, grid.index = y@grid.index, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("growthpixels","list")
  return(res)
}
fordynpoints<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
  
  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", control = control, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("fordynpoints","list")
  return(res)
}
fordyngrid<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
  
  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", control = control, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(grid = y@grid, bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("fordyngrid","list")
  return(res)
}
fordynpixels<-function(y, SpParams, meteo, control = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
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
  
  l = .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", control = control, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
  res = list(coords = y@coords, coords.nrs = y@coords.nrs, grid = y@grid, grid.index = y@grid.index, 
             bbox = y@bbox, proj4string = y@proj4string, 
             xlist = l$xlist, resultlist = l$resultlist, summarylist = l$summarylist)
  class(res) = c("fordynpixels","list")
  return(res)
}