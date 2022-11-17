.f_spatial_day<-function(xi, meteo, date, model){
  res = NULL
  if(inherits(meteo,"data.frame")) met = meteo[date,,drop = FALSE]
  else if(inherits(meteo,"SpatialGridMeteorology") || inherits(meteo,"SpatialPixelsMeteorology")) {
    met = meteoland::extractgridpoints(meteo, as(xi$point, "Spatial"))
    met = met@data[[1]]
  } 
  else if(inherits(meteo, "MeteorologyInterpolationData")) {
    spt = SpatialPointsTopography(as(xi$point, "Spatial"), 
                                  elevation = xi$elevation, 
                                  slope = xi$slope, 
                                  aspect = xi$aspect)
    met = meteoland::interpolationpoints(meteo, spt, dates=as.Date(date), verbose=FALSE)
    met = met@data[[1]]
  }
  else if(inherits(meteo, "stars")) {
    pt_sf = sf::st_sf(geometry = xi$point, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
    met = meteoland::interpolate_data(pt_sf, meteo, dates = as.Date(date))
    met = met$interpolated_data[[1]]
    met = as.data.frame(met)
    row.names(met) = met$dates
    met$dates = NULL
  }    
  
  tmin = met[date,"MinTemperature"]
  tmax = met[date,"MaxTemperature"]
  rhmin = met[date,"MinRelativeHumidity"]
  rhmax = met[date,"MaxRelativeHumidity"]
  rad = met[date,"Radiation"]
  wind = met[date,"WindSpeed"]
  prec = met[date,"Precipitation"]
  if(model=="spwb") {
    if(inherits(xi$x, "spwbInput")){
      res<-medfate::spwb_day(xi$x, date, 
                             tmin = tmin, tmax = tmax, rhmin = rhmin, rhmax = rhmax, rad = rad, wind = wind,
                             latitude = xi$latitude, elevation = xi$elevation,
                             slope = xi$slope, aspect = xi$aspect, prec = prec,
                             modifyInput = TRUE)
    } 
  } else if(model=="growth") {
    if(inherits(xi$x, "growthInput")) {
      res<-medfate::growth_day(xi$x, date, 
                               tmin = tmin, tmax = tmax, rhmin = rhmin, rhmax = rhmax, rad = rad, wind = wind,
                               latitude = xi$latitude, elevation = xi$elevation,
                               slope = xi$slope, aspect = xi$aspect, prec = prec,
                               modifyInput = TRUE)
    } 
  } 
  return(res)
}

.model_spatial_day<-function(y, meteo, date, model = "spwb", 
                            SpParams, 
                            localControl = defaultControl(),
                            parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL,
                            progress = TRUE) {
  
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]
  
  localControl$verbose = FALSE
  
  forestlist = y$forest
  soillist  = y$soil
  xlist  = y$state

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
                     id = y$id[i], 
                     point = sf::st_geometry(y)[i], x = xlist[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i])
    }
    if(progress) cat(paste0("  ii) Parallel computation (cores = ", numCores, ", chunk size = ", chunk.size,")\n"))
    cl<-parallel::makeCluster(numCores)
    reslist_parallel = parallel::parLapplyLB(cl, XI, .f_spatial_day, 
                                             meteo = meteo, date = date, model = model,
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
      xi = list(i = i, id = y$id[i],
                point = sf::st_geometry(y)[i], x = xlist[[i]],
                latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i])
      resultlist[[i]] = .f_spatial_day(xi, meteo = meteo, date = date, model = model)
    }
  }
  res = sf::st_sf(geometry=sf::st_geometry(y))
  res$state = xlist
  res$result = resultlist
  return(sf::st_as_sf(tibble::as_tibble(res)))
}

#' One-day simulation for spatially-distributed forest stands
#' 
#' Functions that allow calling local models \code{\link{spwb_day}} or \code{\link{growth_day}}, 
#' for a set of forest stands distributed in specific locations and a given date. 
#' No spatial processes are simulated.
#'
#' @param y An object of class \code{\link{sf}} with landscape information.
#' @param meteo Meteorology data (see \code{\link{spwb_spatial}}).
#' @param date A string with the date to be simulated.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param localControl A list of local model control parameters (see \code{\link{defaultControl}}).
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param numCores Integer with the number of cores to be used for parallel computation.
#' @param chunk.size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' 
#' @returns An object of class \code{\link{sf}} the same name as the function called containing three elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{state}: A list of model input objects for each simulated stand, to be used in subsequent simulations.}
#'   \item{\code{result}: A list of model output for each simulated stand.}
#' }
#' 
#' @details Simulation functions accept different formats for meteorological input (parameter \code{meteo}) 
#' as described in \code{\link{spwb_spatial}}
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{spwb_day}}, \code{\link{growth_day}}, \code{\link{spwb_spatial}}
#' 
#' @examples 
#' \dontrun{
#' #Load example landscape data
#' data("examplepointslandscape")
#'   
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplepointslandscape)
#'
#' #Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' #Load default medfate parameters
#' data("SpParamsMED")
#'   
#' #Perform simulation
#' date = "2001-03-01"
#' res = spwb_spatial_day(y, examplemeteo, date, SpParamsMED)
#' }
#' @name spwb_spatial_day
spwb_spatial_day<-function(y, meteo, date, SpParams, localControl = defaultControl(),
                         parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .check_model_inputs(y, meteo)
  .model_spatial_day(y=y, meteo = meteo, date = date, model = "spwb", SpParams = SpParams, localControl = localControl, 
                    parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}
#' @rdname spwb_spatial_day
growth_spatial_day<-function(y, meteo, date, SpParams, localControl = defaultControl(),
                           parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .check_model_inputs(y, meteo)
  .model_spatial_day(y=y, meteo = meteo, date = date, model = "growth", SpParams = SpParams, localControl = localControl,
                    parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}