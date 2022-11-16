.f_spatial<-function(xi, meteo, dates, model, sp_class,
                     SpParams, localControl, CO2ByYear = numeric(0), 
                     summaryFunction = NULL, summaryArgs = NULL){
  f = xi$forest
  s = xi$soil
  x = xi$x

  f_out = NULL
  res = NULL
  if(inherits(meteo,"data.frame")) met = meteo
  else if(inherits(meteo, "character")) {
    if(sp_class == "SpatialPoints") {
      if(length(meteo)==1) met = meteoland::readmeteorologypoints(meteo, stations = xi$id, dates = dates)
      else met = meteoland::readmeteorologypoints(meteo, stations = xi$id)
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
    met = meteoland::interpolationpoints(meteo, xi$spt, dates=dates, verbose=FALSE)
    met = met@data[[1]]
  }
  if(!is.null(dates)) met = met[as.character(dates),,drop =FALSE] #subset dates
  if(model=="spwb") {
    if(inherits(x, "spwbInput")){
      try({res<-medfate::spwb(x, meteo=met,
                         latitude = xi$latitude, elevation = xi$elevation,
                         slope = xi$slope, aspect = xi$aspect,
                         CO2ByYear = CO2ByYear)})
    } 
  } else if(model=="growth") {
    if(inherits(x, "growthInput")) {
      try({res<-medfate::growth(x, meteo=met,
                           latitude = xi$latitude, elevation = xi$elevation,
                           slope = xi$slope, aspect = xi$aspect,
                           CO2ByYear = CO2ByYear)})
    } 
  } else if(model=="fordyn") {
    if(inherits(f, "forest") && inherits(s, "soil")) {
      try({res<-medfate::fordyn(forest = f, soil = s, SpParams = SpParams, meteo=met, control = localControl,
                           latitude = xi$latitude, elevation = xi$elevation,
                           slope = xi$slope, aspect = xi$aspect,
                           CO2ByYear = CO2ByYear,
                           management_function = xi$managementFunction, management_args = xi$managementArgs)})
    }
  } 
  if(!is.null(summaryFunction) && !is.null(res)){
    argList = list(object=res)
    if(!is.null(summaryArgs)) argList = c(argList, summaryArgs)
    s = do.call(summaryFunction, args=argList)
  } else {
    s = NULL
  }
  if(model=="fordyn"){
    fs_i = res$ForestStructures
    f_out = fs_i[[length(fs_i)]]
  }
  return(list(result = res, summary = s, forest_out = f_out))
}

.check_model_inputs<-function(y, meteo) {
  if(!inherits(y, "sf")) stop("'y' has to be an object of class 'sf'.")
  if(!all(c("elevation","slope","aspect") %in% names(y))) stop("Columns 'elevation', 'slope' and 'aspect' must be defined.")
  if(!("forest" %in% names(y))) stop("Column 'forest' must be defined.")
  if(!("soil" %in% names(y))) stop("Column 'soil' must be defined.")
  # if(inherits(y, "SpatialGridLandscape")) {
  #   if(!inherits(meteo,c("data.frame","SpatialGridMeteorology","MeteorologyInterpolationData")))
  #     stop("'meteo' has to be of class 'data.frame', 'SpatialGridMeteorology' or 'MeteorologyInterpolationData'.")
  #   if(inherits(meteo,"SpatialGridMeteorology")) {
  #     ycoords = coordinates(y)
  #     mcoords = coordinates(meteo)
  #     if(sum(ycoords == mcoords)!=2*nrow(ycoords)) stop("Coordinates of 'y' and 'meteo' must be the same.")
  #   }
  # } else if(inherits(y, "SpatialPixelsLandscape")) {
  #   if(!inherits(meteo,c("data.frame","character","SpatialPixelsMeteorology","MeteorologyInterpolationData")))
  #     stop("'meteo' has to be of class 'data.frame', 'SpatialPixelsMeteorology' or 'MeteorologyInterpolationData'.")
  #   if(inherits(meteo,"SpatialPixelsMeteorology")) {
  #     ycoords = coordinates(y)
  #     mcoords = coordinates(meteo)
  #     if(round(sum(abs(as.vector(ycoords) - as.vector(mcoords)))) > 0) stop("Coordinates of 'y' and 'meteo' must be the same.")
  #   }
  # } else if(inherits(y, "SpatialPointsLandscape")) {
  #   if(!inherits(meteo,c("data.frame","character","SpatialPointsMeteorology","MeteorologyInterpolationData")))
  #     stop("'meteo' has to be of class 'data.frame', 'character', 'SpatialPointsMeteorology' or 'MeteorologyInterpolationData'.")
  #   if(inherits(meteo,"SpatialPointsMeteorology")) {
  #     ycoords = coordinates(y)
  #     mcoords = coordinates(meteo)
  #     if(round(sum(abs(as.vector(ycoords) - as.vector(mcoords)))) > 0) stop("Coordinates of 'y' and 'meteo' must be the same.")
  #   }
  # } 
  if(inherits(meteo, "character")) {
    if(!all(file.exists(meteo))) stop("Some strings do not correspond to file names")
  }
}

.model_spatial<-function(y, SpParams, meteo, model = "spwb",
                        localControl = defaultControl(), dates = NULL,
                        managementFunction = NULL,
                        CO2ByYear = numeric(0), keepResults = TRUE,
                        summaryFunction=NULL, summaryArgs=NULL,
                        parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL,
                        progress = TRUE) {
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]

  localControl$verbose = FALSE

  forestlist = y$forest
  soillist  = y$soil
  xlist  = y$state
  managementArgs = y$managementarguments

  n = length(forestlist)
  resultlist = vector("list",n)
  summarylist = vector("list",n)
  forestlist_out = vector("list",n)

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

  if(progress) cat(paste0("Simulation of model '", model,"' on ",n," locations:\n"))
  if(parallelize) {
    if(progress) cat("   i) Preparation\n")
    
    if(is.null(chunk.size)) chunk.size = floor(n/numCores)
        
    XI = vector("list", n)
    for(i in 1:n) {
      ma = NULL
      if(!is.null(managementArgs)) ma = managementArgs[[i]]
      XI[[i]] = list(i = i, 
                     id = y$id[i], 
                     point = sf::st_geometry(y)[i],
                     forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                     managementFunction = managementFunction, managementArgs = y$managementarguments[[i]])
    }
    if(progress) cat(paste0("  ii) Parallel computation (cores = ", numCores, ", chunk size = ", chunk.size,")\n"))
    cl<-parallel::makeCluster(numCores)
    reslist_parallel = parallel::parLapplyLB(cl, XI, .f_spatial, 
                                             meteo = meteo, dates = dates, model = model,
                                             SpParams = SpParams, localControl = localControl, CO2ByYear = CO2ByYear,
                                             summaryFunction = summaryFunction, 
                                             chunk.size = chunk.size)
    parallel::stopCluster(cl)
    if(progress) cat(" iii) Retrieval\n")
    for(i in 1:n) {
      if(!is.null(reslist_parallel[[i]]$result)) {
        if(model=="spwb") xlist[[i]] = reslist_parallel[[i]]$result$spwbOutput
        else if(model=="growth") xlist[[i]] = reslist_parallel[[i]]$result$growthOutput
        else if(model=="fordyn") xlist[[i]] = reslist_parallel[[i]]$result$NextInputObject
      }
      if(keepResults && !is.null(reslist_parallel[[i]]$result)) resultlist[[i]] = reslist_parallel[[i]]$result
      if(!is.null(reslist_parallel[[i]]$summary)) summarylist[[i]] = reslist_parallel[[i]]$summary
      if(model=="fordyn" && !is.null(reslist_parallel[[i]]$forest_out)) forestlist_out[[i]] = reslist_parallel[[i]]$forest_out
    }
    if(progress) cat("\n")
  } else {
    if(progress) pb = txtProgressBar(0, n, style=3)
    for(i in 1:n) {
      if(progress) setTxtProgressBar(pb, i)
      xi = list(i = i, 
                id = y$id[i],
                point = sf::st_geometry(y)[i],
                forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                managementFunction = managementFunction, managementArgs = y$managementarguments[[i]])
      sim_out = .f_spatial(xi = xi, 
                      meteo = meteo, dates = dates, model = model, sp_class = sp_class,
                      SpParams = SpParams, localControl = localControl, CO2ByYear = CO2ByYear, 
                      summaryFunction = summaryFunction, summaryArgs = summaryArgs)
      if(!is.null(sim_out$result)) {
        if(model=="spwb") xlist[[i]] = sim_out$result$spwbOutput
        else if(model=="growth") xlist[[i]] = sim_out$result$growthOutput
        else if(model=="fordyn") xlist[[i]] = sim_out$result$NextInputObject
      }
      if(keepResults && !is.null(sim_out$result)) resultlist[[i]] = sim_out$result
      if(!is.null(sim_out$summary)) summarylist[[i]] = sim_out$summary
      if(model=="fordyn" && !is.null(sim_out$forest_out)) forestlist_out[[i]] = sim_out$forest_out
    }
  }
  res = sf::st_sf(geometry=sf::st_geometry(y))
  res$state = xlist
  if(model=="fordyn") res$forest = forestlist_out
  res$result = resultlist
  res$summary = summarylist
  return(sf::st_as_sf(tibble::as_tibble(res)))
}

#' Simulations for spatially-distributed forest stands
#' 
#' Functions that allow calling local models \code{\link{spwb}}, \code{\link{growth}} or \code{\link{fordyn}}, for a set of forest stands distributed in specific locations. 
#' No spatial processes are simulated.
#' 
#' @param y An object of class \code{\link{sf}}.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param meteo Input meteorological data (see section details).
#' @param localControl A list of control parameters (see \code{\link{defaultControl}}) for function \code{\link{spwb_day}} or \code{\link{growth_day}}.
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param keepResults Boolean flag to indicate that point/cell simulation results are to be returned (set to \code{FALSE} and use summary functions for large data sets).
#' @param summaryFunction An appropriate function to calculate summaries (e.g., \code{\link{summary.spwb}}).
#' @param summaryArgs List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param numCores Integer with the number of cores to be used for parallel computation.
#' @param chunk.size Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' @param managementFunction A function that implements forest management actions (see \code{\link{fordyn}}).
#' of such lists, one per spatial unit.
#' 
#' @details Simulation functions  accept different formats for meteorological input (parameter \code{meteo}). The user may supply four kinds of weather sources: 
#' \enumerate{
#'   \item{An object of \code{\link{SpatialPixelsMeteorology-class}}.}
#'   \item{An object of \code{\link{MeteorologyInterpolationData-class}}.}
#'   \item{A data frame with information regarding where to read meteorological data.}
#'   \item{A data frame with meteorological data common for all cells of the grid.}
#'   }
#'  In the case of (1), all the spatio-temporal variation of weather is already supplied by the user. 
#'  In the case of (2), interpolation of weather is done over each grid cell every simulated day. 
#'  In the case of (3) weather maps are read for each day. 
#'  Finally, in the case of (4) spatial variation of weather is not considered.
#'  
#' @returns An object of class 'sf' containing four elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{state}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{updateState}}).}
#'   \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{updateState}}).}
#'   \item{\code{result}: A list of model output for each simulated stand (if \code{keepResults = TRUE}).}
#'   \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summaryFunction} was not \code{NULL}).}
#' }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link{spwb}}, \code{\link{growth}}, \code{\link{fordyn}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{summary_spatial}} , \code{\link{plot_summary}}, \code{\link{update_state}}
#' 
#' @examples
#' \dontrun{
#' # Load example landscape data
#' data("examplepointslandscape")
#'   
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplepointslandscape)
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Perform simulation
#' dates = seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
#' res = spwb_spatial(y, SpParamsMED, examplemeteo, dates = dates)
#'   
#' # Generate summaries (these could have also been specified when calling 'spwbspatial')
#' res_sum = simulation_summary(res, summaryFunction = summary.spwb, freq="month")
#' 
#' # Plot summaries
#' plot_summary(res_sum, "Transpiration", "2001-03-01")
#' }
#' 
#' @name spwb_spatial
spwb_spatial<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                     CO2ByYear = numeric(0), keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                     parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .check_model_inputs(y, meteo)
  .model_spatial(y=y, SpParams = SpParams, meteo = meteo, model = "spwb", localControl = localControl, dates = dates,
                CO2ByYear = CO2ByYear, keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}

#' @rdname spwb_spatial
growth_spatial<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .check_model_inputs(y, meteo)
  .model_spatial(y=y, SpParams = SpParams, meteo = meteo, model = "growth", localControl = localControl, dates = dates,
                CO2ByYear = CO2ByYear, keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}

#' @rdname spwb_spatial
fordyn_spatial<-function(y, SpParams, meteo, localControl = defaultControl(), dates = NULL,
                       managementFunction = NULL,
                       CO2ByYear = numeric(0), keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE) {
  .check_model_inputs(y, meteo)
  .model_spatial(y=y, SpParams = SpParams, meteo = meteo, model = "fordyn", localControl = localControl, dates = dates,
                managementFunction = managementFunction,
                CO2ByYear = CO2ByYear, keepResults = keepResults, summaryFunction = summaryFunction, summaryArgs = summaryArgs, 
                parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
}