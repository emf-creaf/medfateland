.f_spatial_day<-function(xi, meteo, date, model, sp_class){
  x = xi$x
  res = NULL
  if(inherits(meteo,"data.frame")) met = meteo[date,,drop = FALSE]
  else if(inherits(meteo, "character")) {
    if(sp_class == "SpatialPoints") {
      met = meteoland::readmeteorologypoints(meteo, stations = xi$id, dates = date)
    } else {
      met = meteoland::extractgridpoints(meteo, as(xi$spt, "SpatialPoints"))
    }
    met = met@data[[1]]
  }
  else if(inherits(meteo,"SpatialPointsMeteorology")) {
    met = meteo@data[[xi$i]]
  } 
  else if(inherits(meteo,"SpatialGridMeteorology") || inherits(meteo,"SpatialPixelsMeteorology")) {
    met = meteoland::extractgridpoints(meteo, as(xi$spt, "SpatialPoints"))
    met = met@data[[1]]
  } 
  else if(inherits(meteo, "MeteorologyInterpolationData")) {
    met = meteoland::interpolationpoints(meteo, xi$spt, dates=date, verbose=FALSE)
    met = met@data[[1]]
  }
  tmin = met[date,"MinTemperature"]
  tmax = met[date,"MaxTemperature"]
  rhmin = met[date,"MinRelativeHumidity"]
  rhmax = met[date,"MaxRelativeHumidity"]
  rad = met[date,"Radiation"]
  wind = met[date,"WindSpeed"]
  prec = met[date,"Precipitation"]
  if(model=="spwb") {
    if(inherits(x, "spwbInput")){
      res<-medfate::spwb_day(x, date, 
                             tmin = tmin, tmax = tmax, rhmin = rhmin, rhmax = rhmax, rad = rad, wind = wind,
                             latitude = xi$latitude, elevation = xi$elevation,
                             slope = xi$slope, aspect = xi$aspect, prec = prec,
                             modifyInput = TRUE)
    } 
  } else if(model=="growth") {
    if(inherits(x, "growthInput")) {
      res<-medfate::growth_day(x, date, 
                               tmin = tmin, tmax = tmax, rhmin = rhmin, rhmax = rhmax, rad = rad, wind = wind,
                               latitude = xi$latitude, elevation = xi$elevation,
                               slope = xi$slope, aspect = xi$aspect, prec = prec,
                               modifyInput = TRUE)
    } 
  } 
  return(res)
}

.modelspatial_day<-function(y, meteo, date, model = "spwb", 
                            SpParams, 
                            localControl = defaultControl(),
                            parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL,
                            progress = TRUE) {
  
  if(inherits(y, "SpatialGrid")) {
    sp_class = "SpatialGrid"
  } else if(inherits(y, "SpatialPixels")) {
    sp_class = "SpatialPixels"
  } else if(inherits(y, "SpatialPoints")) {
    sp_class = "SpatialPoints"
  }
  
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
      if(progress) cat(paste0("All input objects are already available for model '", model, "'.\n\n"))
    }
  }


  if(progress) cat(paste0("Simulation of model '",model, "' on ",n," locations for date '", date,"':\n"))
  if(parallelize) {
    if(progress) cat("   i) Preparation\n")
    
    if(is.null(chunk.size)) chunk.size = floor(n/numCores)
    
    XI = vector("list", n)
    for(i in 1:n) {
      XI[[i]] = list(i = i, 
                     id = names(forestlist)[i], 
                     spt = spt[i], x = xlist[[i]],
                     latitude = latitude[i], elevation = elevation[i], slope= slope[i], aspect = aspect[i])
    }
    if(progress) cat(paste0("  ii) Parallel computation (cores = ", numCores, ", chunk size = ", chunk.size,")\n"))
    cl<-parallel::makeCluster(numCores)
    reslist_parallel = parallel::parLapplyLB(cl, XI, .f_spatial_day, 
                                             meteo = meteo, date = date, model = model, sp_class = sp_class,
                                             chunk.size = chunk.size)
    parallel::stopCluster(cl)
    if(progress) cat(" iii) Retrieval\n")
    for(i in 1:n) {
      resultlist[[i]] = reslist_parallel[[i]]
    }
    if(progress) cat("\n")
  } else {
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      xi = list(i = i, id = names(forestlist)[i],
                spt = spt[i], x = xlist[[i]],
                latitude = latitude[i], elevation = elevation[i], slope= slope[i], aspect = aspect[i])
      resultlist[[i]] = .f_spatial_day(xi, meteo = meteo, date = date, model = model, sp_class = sp_class)
    }
  }

  res = list(sp = as(y, sp_class), 
             xlist = xlist, resultlist = resultlist)
  class(res) = c(paste0(model, "spatial", "_day"),"list")
  return(res)
}

spwbspatial_day<-function(y, meteo, date, SpParams, localControl = defaultControl(),
                         parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .checkmodelinputs(y, meteo)
  .modelspatial_day(y=y, meteo = meteo, date = date, model = "spwb", SpParams = SpParams, localControl = localControl, 
                    parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}
growthspatial_day<-function(y, meteo, date, SpParams, localControl = defaultControl(),
                           parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .checkmodelinputs(y, meteo)
  .modelspatial_day(y=y, meteo = meteo, date = date, model = "growth", SpParams = SpParams, localControl = localControl,
                    parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}