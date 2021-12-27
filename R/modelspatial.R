.modelspatial<-function(y, SpParams, meteo, model = "spwb",
                        localControl = defaultControl(), dates = NULL,
                        mergeTrees = FALSE, keepResults = TRUE,
                        summaryFunction=NULL, args=NULL,
                        parallelize = FALSE, progress = TRUE) {
  spts = as(y,"SpatialPoints")
  topo = y@data
  spt = SpatialPointsTopography(spts, topo$elevation, topo$slope, topo$aspect)
  longlat = spTransform(spts,CRS(SRS_string = "EPSG:4326"))
  latitude = longlat@coords[,2]
  elevation = y@data$elevation
  slope = y@data$slope
  aspect = y@data$aspect

  localControl$verbose = FALSE

  forestlist = y@forestlist
  soillist  = y@soillist
  xlist  = y@xlist


  n = length(forestlist)
  resultlist = vector("list",n)
  summarylist = vector("list",n)
  names(resultlist) = names(forestlist)
  names(summarylist) = names(forestlist)
  
  initf<-function(i) {
    f = forestlist[[i]]
    s = soillist[[i]]
    x = NULL
    if(inherits(f, "forest") && inherits(s, "soil")) {
      if(mergeTrees) f = forest_mergeTrees(f)
      if(model=="spwb") {
        x = medfate::forest2spwbInput(f, s, SpParams, localControl)
      } else if(model=="growth") {
        x = medfate::forest2growthInput(f, s, SpParams, localControl)
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
    else if(inherits(meteo, "character")) {
      met = meteoland::readmeteorologypointfiles(meteo, stations = names(forestlist)[i])
      met = met@data[[1]]
    }
    else if(inherits(meteo,"SpatialPointsMeteorology") || inherits(meteo,"SpatialGridMeteorology")|| inherits(meteo,"SpatialPixelsMeteorology")) {
      met = meteo@data[[i]]
    } else if(inherits(meteo, "MeteorologyInterpolationData")) {
      met = meteoland::interpolationpoints(meteo, spt[i], dates=dates, verbose=FALSE)
      met = met@data[[1]]
    }
    if(!is.null(dates)) met = met[as.character(dates),,drop =FALSE] #subset dates
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
        res<-medfate::fordyn(forest = f, soil = s, SpParams = SpParams, meteo=met, localControl = localControl,
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
                "SpParams", "localControl", "latitude", "elevation", "slope", "aspect",
                "dates", "args", "xlist")
    parallel::clusterExport(cl, varlist, envir = env)
    reslist_parallel = parallel::clusterApply(cl, 1:n, simf, sfun = summaryFunction)
    parallel::stopCluster(cl)
    for(i in 1:n) {
      resultlist[[i]] = reslist_parallel[[i]]$result
      summarylist[[i]] = reslist_parallel[[i]]$summary
    }
    if(progress) cat("done.\n")
  } else {
    if(progress) cat("Simulation:\n")
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      sim_out = simf(i, sfun = summaryFunction)
      if(keepResults) resultlist[[i]] = sim_out$result
      summarylist[[i]] = sim_out$summary
    }
  }
  if(inherits(y, "SpatialGrid")) {
    sp = "grid"
    sp_class = "SpatialGrid"
  } else if(inherits(y, "SpatialPixels")) {
    sp = "pixels"
    sp_class = "SpatialPixels"
  } else if(inherits(y, "SpatialPoints")) {
    sp = "points"
    sp_class = "SpatialPoints"
  }
  res = list(sp = as(y, sp_class), 
             xlist = xlist, resultlist = resultlist, summarylist = summarylist)
  class(res) = c(paste0(model, sp),paste0("summary",sp),"list")
  return(res)
}

.checkmodelinputs<-function(sp, y, meteo) {
  if(sp=="grid") {
    if(!inherits(y,"SpatialGridLandscape"))
      stop("'y' has to be of class 'SpatialGridLandscape'.")
    if(!inherits(meteo,c("data.frame","SpatialGridMeteorology","MeteorologyInterpolationData")))
      stop("'meteo' has to be of class 'data.frame', 'SpatialGridMeteorology' or 'MeteorologyInterpolationData'.")
    if(inherits(meteo,"SpatialGridMeteorology")) {
      ycoords = coordinates(y)
      mcoords = coordinates(meteo)
      if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
    }
  } else if(sp=="pixels") {
    if(!inherits(y,"SpatialPixelsLandscape"))
      stop("'y' has to be of class 'SpatialPixelsLandscape'.")
    if(!inherits(meteo,c("data.frame","character","SpatialPixelsMeteorology","MeteorologyInterpolationData")))
      stop("'meteo' has to be of class 'data.frame', 'SpatialPixelsMeteorology' or 'MeteorologyInterpolationData'.")
    if(inherits(meteo,"SpatialPixelsMeteorology")) {
      ycoords = coordinates(y)
      mcoords = coordinates(meteo)
      if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
    }
  } else if(sp=="points") {
    if(!inherits(y,"SpatialPointsLandscape"))
      stop("'y' has to be of class 'SpatialPointsLandscape'.")
    if(!inherits(meteo,c("data.frame","character","SpatialPointsMeteorology","MeteorologyInterpolationData")))
      stop("'meteo' has to be of class 'data.frame', 'character', 'SpatialPointsMeteorology' or 'MeteorologyInterpolationData'.")
    if(inherits(meteo,"SpatialPointsMeteorology")) {
      ycoords = coordinates(y)
      mcoords = coordinates(meteo)
      if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
    }
  } 
  if(inherits(meteo, "character")) {
    if(!all(file.exists(meteo))) stop("Some strings do not correspond to file names")
  }
}

spwbpoints<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("points", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
spwbgrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                   keepResults = TRUE, summaryFunction=NULL, args=NULL,
                   parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("grid", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
spwbpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("pixels", y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
growthpoints<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("points", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
growthgrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("grid",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
growthpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("pixels",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
fordynpoints<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("points",y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
fordyngrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, args=NULL,
                     parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("grid",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}
fordynpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, args=NULL,
                       parallelize = FALSE, progress = TRUE) {
  .checkmodelinputs("pixels",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                    keepResults = keepResults, summaryFunction = summaryFunction, args = args, parallelize = parallelize, progress = progress)
}