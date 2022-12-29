.f_spatial<-function(xi, meteo, dates, model,
                     SpParams, local_control, CO2ByYear = numeric(0), 
                     keep_results = TRUE,
                     management_function = NULL, summary_function = NULL, summary_arguments = NULL){
  f = xi$forest
  s = xi$soil
  x = xi$x

  f_out = NULL
  m_out = NULL
  x_out = NULL
  res = NULL
  
  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) met = meteo
    else if(inherits(meteo,"SpatialGridMeteorology") || inherits(meteo,"SpatialPixelsMeteorology")) {
      met = meteoland::extractgridpoints(meteo, as(xi$point, "Spatial"))
      met = met@data[[1]]
    } 
    else if(inherits(meteo, "MeteorologyInterpolationData")) {
      spt = SpatialPointsTopography(as(xi$point, "Spatial"), 
                                    elevation = xi$elevation, 
                                    slope = xi$slope, 
                                    aspect = xi$aspect)
      met = meteoland::interpolationpoints(meteo, spt, dates=dates, verbose=FALSE)
      met = met@data[[1]]
    }
    else if(inherits(meteo, "stars")) {
      pt_sf = sf::st_sf(geometry = xi$point, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
      met = meteoland::interpolate_data(pt_sf, meteo, dates = dates, verbose = FALSE)
      met = met$interpolated_data[[1]]
      met = as.data.frame(met)
      row.names(met) = met$dates
      met$dates = NULL
    }    
  } else { # If weather was supplied as part of 'xi' list
    met = xi$meteo
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
    if(inherits(s, "data.frame")) {
      s <- soil(s)
    }
    if(inherits(f, "forest") && inherits(s, "soil")) {
      mf = management_function
      ma = xi$management_args
      if(is.null(ma)) mf = NULL
      try({res<-medfate::fordyn(forest = f, soil = s, SpParams = SpParams, meteo=met, control = local_control,
                           latitude = xi$latitude, elevation = xi$elevation,
                           slope = xi$slope, aspect = xi$aspect,
                           CO2ByYear = CO2ByYear,
                           management_function = mf, management_args = ma)})
    }
  } 
  if(!is.null(summary_function) && !is.null(res)){
    argList = list(object=res)
    if(!is.null(summary_arguments)) argList = c(argList, summary_arguments)
    s = do.call(summary_function, args=argList)
  } else {
    s = NULL
  }
  if(model=="fordyn"){
    fs_i = res$ForestStructures
    f_out = res$NextForestObject
    m_out = res$ManagementArgs
  }
  if(model=="spwb") x_out = res$spwbOutput
  else if(model=="growth") x_out = res$growthOutput
  else if(model=="fordyn") x_out = res$NextInputObject
  
  # Frees memory if detailed results are not required
  if(!keep_results) res = NULL
  return(list(result = res, summary = s, x_out = x_out, forest_out = f_out, management_out = m_out))
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
  if(!is.null(meteo)) {
    if(inherits(meteo, "character")) {
      if(!all(file.exists(meteo))) stop("Some strings do not correspond to file names")
    }
  } else {
    if(!("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'y' if not supplied separately")
  }
}

.model_spatial<-function(y, SpParams, meteo, model = "spwb",
                        local_control = defaultControl(), dates = NULL,
                        CO2ByYear = numeric(0), keep_results = TRUE,
                        management_function = NULL, 
                        summary_function=NULL, summary_arguments=NULL,
                        parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL,
                        progress = TRUE) {
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]

  local_control$verbose = FALSE

  forestlist = y$forest
  n = length(forestlist)
  
  soillist  = y$soil
  if("state" %in% names(y)) {
    xlist  = y$state
  } else {
    xlist = vector("list",n)
  }
  if("management_arguments" %in% names(y)) {
    managementlist  = y$management_arguments
  } else {
    managementlist = vector("list",n)
  }
  if("meteo" %in% names(y)) {
    meteolist = y$meteo
  } else {
    meteolist = vector("list",n)
  }
  
  resultlist = vector("list",n)
  summarylist = vector("list",n)
  forestlist_out = vector("list",n)
  managementlist_out = vector("list",n)

  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      f = forestlist[[i]]
      s = soillist[[i]]
      if(inherits(f, "forest") && inherits(s, c("soil", "data.frame"))) {
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
        if(inherits(s, "data.frame")) {
          s <- soil(s)
        }
        if(inherits(f, "forest") && inherits(s, "soil")) {
          if(model=="spwb") {
            xlist[[i]] = medfate::forest2spwbInput(f, s, SpParams, local_control)
          } else if(model=="growth") {
            xlist[[i]] = medfate::forest2growthInput(f, s, SpParams, local_control)
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
    
    if(is.null(chunk_size)) chunk_size = floor(n/num_cores)
        
    XI = vector("list", n)
    for(i in 1:n) {
      XI[[i]] = list(i = i, 
                     id = y$id[i], 
                     point = sf::st_geometry(y)[i],
                     forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                     meteo = meteolist[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                     management_args = managementlist[[i]])
    }
    if(progress) cat(paste0("  ii) Parallel computation (cores = ", num_cores, ", chunk_size = ", chunk_size,")\n"))
    cl<-parallel::makeCluster(num_cores)
    reslist_parallel = parallel::parLapplyLB(cl, XI, .f_spatial, 
                                             meteo = meteo, dates = dates, model = model,
                                             SpParams = SpParams, local_control = local_control, CO2ByYear = CO2ByYear,
                                             keep_results = keep_results,
                                             management_function = management_function, 
                                             summary_function = summary_function, summary_arguments = summary_arguments,
                                             chunk.size = chunk_size)
    parallel::stopCluster(cl)
    if(progress) cat(" iii) Retrieval\n")
    for(i in 1:n) {
      if(!is.null(reslist_parallel[[i]]$x_out)) xlist[[i]] = reslist_parallel[[i]]$x_out
      if(!is.null(reslist_parallel[[i]]$result)) resultlist[[i]] = reslist_parallel[[i]]$result
      if(!is.null(reslist_parallel[[i]]$summary)) summarylist[[i]] = reslist_parallel[[i]]$summary
      if(model=="fordyn"){
        if(!is.null(reslist_parallel[[i]]$forest_out)) forestlist_out[[i]] = reslist_parallel[[i]]$forest_out
        if(!is.null(reslist_parallel[[i]]$management_out)) managementlist_out[[i]] = reslist_parallel[[i]]$management_out
      }
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
                meteo = meteolist[[i]],
                latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                management_args = managementlist[[i]])
      sim_out = .f_spatial(xi = xi, 
                      meteo = meteo, dates = dates, model = model,
                      SpParams = SpParams, local_control = local_control, CO2ByYear = CO2ByYear, 
                      keep_results = keep_results,
                      management_function = management_function, 
                      summary_function = summary_function, summary_arguments = summary_arguments)
      if(!is.null(sim_out$x_out)) xlist[[i]] = sim_out$x_out
      if(!is.null(sim_out$summary)) summarylist[[i]] = sim_out$summary
      if(!is.null(sim_out$result)) resultlist[[i]] = sim_out$result
      if(model=="fordyn"){
        if(!is.null(sim_out$forest_out)) forestlist_out[[i]] = sim_out$forest_out
        if(!is.null(sim_out$management_out)) managementlist_out[[i]] = sim_out$management_out
      }
    }
  }
  res = sf::st_sf(geometry=sf::st_geometry(y))
  res$id <- y$id
  res$state <- xlist
  if(model=="fordyn") {
    res$forest <- forestlist_out
    if(!is.null(management_function)) res$management_arguments <- managementlist_out
  }
  if(keep_results) res$result <- resultlist
  if(!is.null(summary_function)) res$summary <- summarylist
  return(sf::st_as_sf(tibble::as_tibble(res)))
}

#' Simulations for spatially-distributed forest stands
#' 
#' Functions that allow calling local models \code{\link{spwb}}, \code{\link{growth}} or \code{\link{fordyn}}, for a set of forest stands distributed in specific locations. 
#' No spatial processes are simulated.
#' 
#' @param sf An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link{soil}} or data frames of physical properties.}
#'     \item{\code{state}: Objects of class \code{\link{spwbInput}} or \code{\link{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{management_arguments}: Lists with management arguments (optional, relevant for \code{fordyn_spatial} only).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param meteo Input meteorological data (see section details). If NULL, the function will expect a column 'meteo' in parameter \code{y}.
#' @param local_control A list of control parameters (see \code{\link{defaultControl}}) for function \code{\link{spwb_day}} or \code{\link{growth_day}}.
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param keep_results Boolean flag to indicate that point/cell simulation results are to be returned (set to \code{FALSE} and use summary functions for large data sets).
#' @param summary_function An appropriate function to calculate summaries (e.g., \code{\link{summary.spwb}}).
#' @param summary_arguments List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' @param management_function A function that implements forest management actions (see \code{\link{fordyn}}).
#' of such lists, one per spatial unit.
#' 
#' @details Simulation functions  accept different formats for meteorological input (parameter \code{meteo}). 
#' The user may supply four kinds of weather sources: 
#' \enumerate{
#'   \item{A data frame with meteorological data common for all spatial location (spatial variation of weather not considered).}
#'   \item{An object of class \code{\link{stars}} with interpolation data, created by package \code{\link{meteoland}}.}
#'   \item{DEPRECATED: An object of \code{\link{SpatialPixelsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}. All the spatio-temporal variation of weather is already supplied by the user.}
#'   \item{DEPRECATED: An object of \code{\link{MeteorologyInterpolationData-class}}. Interpolation of weather is performed over each spatial unit every simulated day.}
#'   }
#' Alternatively, the user may leave parameter \code{meteo = NULL} and specify a weather data frame for each element of \code{y}
#' in a column named 'meteo'.
#' 
#' @returns An object of class 'sf' containing four elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{id}: Stand id, taken from the input.}
#'   \item{\code{state}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'   \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in function \code{fordyn_spatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'   \item{\code{management_arguments}: A list of management arguments for each simulated stand (only in function \code{fordyn_spatial} if management function was supplied), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'   \item{\code{result}: A list of model output for each simulated stand (if \code{keep_results = TRUE}).}
#'   \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summary_function} was not \code{NULL}).}
#' }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link{spwb}}, \code{\link{growth}}, \code{\link{fordyn}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{simulation_summary}} , \code{\link{plot_summary}}, \code{\link{update_landscape}}
#' 
#' @examples
#' \dontrun{
#' # Load example landscape data
#' data("examplepointslandscape")
#'   
#' # Transform example to 'sf' 
#' pts_sf <- sp_to_sf(examplepointslandscape)
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Perform simulation
#' dates <- seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
#' res <- spwb_spatial(pts_sf, SpParamsMED, examplemeteo, dates = dates)
#'   
#' # Generate summaries (these could have also been specified when calling 'spwbspatial')
#' res_sum <- simulation_summary(res, summary_function = summary.spwb, freq="month")
#' 
#' # Plot summaries
#' plot_summary(res_sum, "Transpiration", "2001-03-01")
#' 
#' # Fordyn simulation for one year (one stand) without management
#' res_noman <- fordyn_spatial(pts_sf[1,], SpParamsMED, examplemeteo)
#' 
#' # Add management arguments to all stands
#' pts_sf$management_arguments <- vector("list", nrow(pts_sf))
#' for(i in 1:nrow(pts_sf)) pts_sf$management_arguments[[i]] <- defaultManagementArguments()
#' 
#' # Change thinning threshold for stand #1
#' pts_sf$management_arguments[[1]]$thinningThreshold <- 15
#' 
#' # Fordyn simulation for one year (one stand) with management
#' res_man <- fordyn_spatial(pts_sf[1,], SpParamsMED, examplemeteo,
#'                           management_function = defaultManagementFunction)
#' 
#' # Compare table of cuttings with vs. without management
#' res_noman$result[[1]]$CutTreeTable
#' res_man$result[[1]]$CutTreeTable
#' }
#' 
#' @name spwb_spatial
spwb_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                     CO2ByYear = numeric(0), keep_results = TRUE, summary_function=NULL, summary_arguments=NULL,
                     parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  .check_model_inputs(sf, meteo)
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "spwb", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}

#' @rdname spwb_spatial
growth_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), keep_results = TRUE, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  .check_model_inputs(sf, meteo)
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "growth", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}

#' @rdname spwb_spatial
fordyn_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), keep_results = TRUE, 
                       management_function = NULL, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  .check_model_inputs(sf, meteo)
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "fordyn", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, 
                management_function = management_function, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}
