.modelspatial<-function(y, SpParams, meteo, model = "spwb",
                        localControl = defaultControl(), dates = NULL,
                        managementFunction = NULL, managementArgs = NULL,
                        keepResults = TRUE,
                        summaryFunction=NULL, summaryArgs=NULL,
                        parallelize = FALSE, numCores = detectCores()-1,
                        progress = TRUE) {
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
  

  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      f = forestlist[[i]]
      s = soillist[[i]]
      if(inherits(f, "forest") && inherits(s, "soil")) {
        init[i] = TRUE
        x = xlist[[i]]
        if(inherits(x,"spwbInput") && model=="spwb") init[i] = FALSE
        if(inherits(x,"growthInput") && model=="growth") init[i] = FALSE
      }
    }
    w_init = which(init)
    if(length(w_init)>0) {
      if(progress) cat(paste0("Creating ", length(w_init)," input objects for model '", model, "':\n"))
      if(progress) pb = txtProgressBar(0, length(w_init), style=3)
      for(w in 1:length(w_init)) {
        if(progress) setTxtProgressBar(pb, w)
        i = w_init[w]
        f = forestlist[[i]]
        s = soillist[[i]]
        if(inherits(f, "forest") && inherits(s, "soil")) {
          if(model=="spwb") {
            xlist[[i]] = medfate::forest2spwbInput(f, s, SpParams, localControl)
          } else if(model=="growth") {
            xlist[[i]] = medfate::forest2growthInput(f, s, SpParams, localControl)
          }
        }
      }
      if(progress) cat("\n\n")
    } else {
      if(progress) cat(paste0("All input objects are already available for '", model, "'.\n\n"))
    }
  }

  simf<-function(xi, sfun = NULL, mfun = NULL){
    f = xi$forest
    s = xi$soil
    x = xi$x
    res = NULL
    if(inherits(meteo,"data.frame")) met = meteo
    else if(inherits(meteo, "character")) {
      if(length(meteo)==1) met = meteoland::readmeteorologypoints(meteo, stations = xi$id, dates = dates)
      else met = meteoland::readmeteorologypoints(meteo, stations = xi$id)
      met = met@data[[1]]
    }
    else if(inherits(meteo,"SpatialPointsMeteorology") || inherits(meteo,"SpatialGridMeteorology")|| inherits(meteo,"SpatialPixelsMeteorology")) {
      met = meteo@data[[xi$i]]
    } else if(inherits(meteo, "MeteorologyInterpolationData")) {
      met = meteoland::interpolationpoints(meteo, xi$spt, dates=dates, verbose=FALSE)
      met = met@data[[1]]
    }
    if(!is.null(dates)) met = met[as.character(dates),,drop =FALSE] #subset dates
    if(model=="spwb") {
      if(inherits(x, "spwbInput")){
        res<-medfate::spwb(x, meteo=met,
                           latitude = xi$latitude, elevation = xi$elevation,
                           slope = xi$slope, aspect = xi$aspect)
      } 
    } else if(model=="growth") {
      if(inherits(x, "growthInput")) {
        res<-medfate::growth(x, meteo=met,
                             latitude = xi$latitude, elevation = xi$elevation,
                             slope = xi$slope, aspect = xi$aspect)
      } 
    } else if(model=="fordyn") {
      if(inherits(f, "forest") && inherits(s, "soil")) {
        res<-medfate::fordyn(forest = f, soil = s, SpParams = SpParams, meteo=met, control = localControl,
                             latitude = xi$latitude, elevation = xi$elevation,
                             slope = xi$slope, aspect = xi$aspect,
                             management_function = mfun, management_args = managementArgs)
      }
    } 
    if(!is.null(sfun) && !is.null(res)){
      argList = list(object=res)
      if(!is.null(summaryArgs)) {
        for(j in 1:length(summaryArgs)) argList[names(summaryArgs)[j]]=summaryArgs[j]
      }
      s = do.call(sfun, args=argList)
    } else {
      s = NULL
    }
    return(list(result = res, summary = s))
  }

  if(parallelize) {
    if(progress) cat(paste0("Simulation of model '", model,"' on ",n," locations for ", length(dates)," days (", numCores, " cores):"))
    env<-environment()
    if(progress) cat(" preparation")
    XI = vector("list", n)
    for(i in 1:n) {
      XI[[i]] = list(i = i, id = names(forestlist)[i], spt = spt[i],
                     forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                     latitude = latitude[i], elevation = elevation[i], slope= slope[i], aspect = aspect[i])
    }
    if(progress) cat(", computation")
    cl<-parallel::makeCluster(numCores)
    varlist = c("meteo", "model","SpParams", "localControl", 
                "dates", "summaryArgs", "managementArgs")
    parallel::clusterExport(cl, varlist, envir = env)
    reslist_parallel = parallel::parLapply(cl, XI, simf, sfun = summaryFunction, mfun = managementFunction)
    parallel::stopCluster(cl)
    if(progress) cat(", retrieval")
    for(i in 1:n) {
      resultlist[[i]] = reslist_parallel[[i]]$result
      summarylist[[i]] = reslist_parallel[[i]]$summary
    }
    if(progress) cat(".\n")
  } else {
    if(progress) cat(paste0("Simulation of model '", model,"' on ",n," locations for ", length(dates)," days:\n"))
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      xi = list(i = i, id = names(forestlist)[i],
                spt = spt[i],
                forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                latitude = latitude[i], elevation = elevation[i], slope= slope[i], aspect = aspect[i])
      sim_out = simf(xi = xi, sfun = summaryFunction, mfun = managementFunction)
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
                     keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                     parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("points", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
spwbgrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                   keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                   parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("grid", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
spwbpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                     parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("pixels", y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
growthpoints<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("points", y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
growthgrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                     parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("grid",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
growthpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("pixels",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
fordynpoints<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       managementFunction = NULL, managementArgs = NULL,
                       keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("points",y, meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                managementFunction = managementFunction, managementArgs = managementArgs,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
fordyngrid<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     managementFunction = NULL, managementArgs = NULL,
                     keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                     parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("grid",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                managementFunction = managementFunction, managementArgs = managementArgs,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}
fordynpixels<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       managementFunction = NULL, managementArgs = NULL,
                       keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, progress = TRUE) {
  .checkmodelinputs("pixels",y,meteo)
  .modelspatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                managementFunction = managementFunction, managementArgs = managementArgs,
                keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, progress = progress)
}