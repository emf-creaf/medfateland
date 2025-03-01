.f_optim_rock<-function(xi, meteo, dates, model,
                        SpParams, local_control, 
                        PLCquantile = 0.9, qPLC_target = 12, qPLC_tol = 0.5, max_rocks = 99) {

  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) { # A data frame common for all locations
      met <- meteo
    }
    else if(inherits(meteo, "stars")) { # An interpolator object
      pt_sf <- sf::st_sf(geometry = xi$point, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
      met <- meteoland::interpolate_data(pt_sf, meteo, dates = dates, 
                                         verbose = FALSE, ignore_convex_hull_check = TRUE)
      met <- met$interpolated_data[[1]]
    }
    else if(inherits(meteo, "list")) { # A list of interpolators
      pt_sf <- sf::st_sf(geometry = xi$point, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
      met <- data.frame()
      for(i in 1:length(meteo)) {
        interpolator_i <- meteo[[i]]
        dates_i <- as.Date(stars::st_get_dimension_values(interpolator_i, "date"))
        if(!is.null(dates)) dates_i <- dates_i[dates_i %in% dates]
        if(length(dates_i)>0) { # Only call interpolation if there are matching dates 
          met_i <- meteoland::interpolate_data(pt_sf, meteo[[i]], dates = dates_i, 
                                               verbose = FALSE, ignore_convex_hull_check = TRUE)
          met <- rbind(met, met_i$interpolated_data[[1]])
        }
      }
    }
  } else { # If weather was supplied as part of 'xi' list
    met <- xi$meteo
  }
  if(!("dates" %in% names(met))) {
    if(!is.null(dates)) met <- met[as.character(dates),,drop =FALSE] #subset dates
    met$dates <- as.Date(row.names(met))
    row.names(met) <- NULL
  } else {
    if(!is.null(dates)) met <- met[as.character(met$dates) %in% as.character(dates),,drop =FALSE] #subset dates
    met$dates <- as.Date(met$dates)
  }
  ro <- utils_rockOptimization(x = xi$forest, 
                               soil = xi$soil, 
                               SpParams = SpParams, 
                               control = local_control,
                               meteo = met,
                               PLCquantile = PLCquantile, qPLC_target = qPLC_target, qPLC_tol = qPLC_tol, max_rocks = max_rocks,
                               latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
  return(ro)
}

#' Rock optimization
#'
#' Optimization of rock fragment content 
#'
#' @param sf An object of class \code{\link[sf]{sf}} (see \code{\link{spwb_spatial}}).
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param meteo Input meteorological data (see section details in \code{\link{spwb_spatial}}).
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}}).
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param PLCquantile Maximum PLC quantile to be calculated across years.
#' @param qPLC_target Target PLC to be achieved (by default 12%).
#' @param qPLC_tol Tolerance of PLC difference to target accepted when finding solution.
#' @param max_rocks Maximum content in coarse fragments allowed for any soil layer.
#' @param progress Boolean flag to display progress information of simulations.
#' 
#' @returns
#'  An object of class \code{\link[sf]{sf}} with a modified \code{soil} column and an additional column
#'  \code{optimization_message} with text information about the optimization.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso
#' \code{\link[medfate]{utils_rockOptimization}}
#' 
#' @examples
#' \donttest{
#' data("example_ifn")
#' data("examplemeteo")
#' data("SpParamsMED")
#' example_subset <- example_ifn[31:32, ]
#' optimization_rock(example_subset, SpParamsMED, examplemeteo)
#' }
#' @name optimization_rock
#' @export
optimization_rock<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                            parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL,
                            PLCquantile = 0.9, qPLC_target = 12, qPLC_tol = 0.5, max_rocks = 99,
                            progress = TRUE) {
  
  
  latitude <- sf::st_coordinates(sf::st_transform(sf::st_geometry(sf),4326))[,2]
  n <- nrow(sf)
  local_control$verbose <- FALSE
  
  if(is.null(meteo) && !("meteo" %in% names(sf))) stop("Column 'meteo' must be defined in 'sf' if not supplied separately")
  if("meteo" %in% names(sf)) {
    if(progress) cli::cli_progress_step(paste0("Checking meteo column input"))
    meteo <- NULL
    meteolist <- .check_meteo_column_input(sf$meteo, dates, FALSE) 
  } else {
    if(progress) cli::cli_progress_step(paste0("Checking meteo object input"))
    meteo <- .check_meteo_object_input(meteo, dates)
    meteolist <- vector("list",n)
  }
  
  if(parallelize) {
    if(progress) {
      cli::cli_progress_step("Preparing data for parallelization")
    }
    num_cores <- min(num_cores, n)
    if(is.null(chunk_size)) chunk_size = max(2, floor(n/num_cores))
    
    XI = vector("list", n)
    for(i in 1:n) {
      XI[[i]] = list(i = i, 
                     point = sf::st_geometry(sf)[i],
                     forest = sf$forest[[i]], soil = sf$soil[[i]],
                     meteo = meteolist[[i]],
                     latitude = latitude[i], elevation = sf$elevation[i], slope= sf$slope[i], aspect = sf$aspect[i])
    }
    if(progress) cli::cli_progress_step(paste0("Launching parallel computation (cores = ", num_cores, "; chunk size = ", chunk_size,")"))
    cl<-parallel::makeCluster(num_cores)
    reslist <- parallel::parLapplyLB(cl, XI, .f_optim_rock, 
                                     meteo = meteo, dates = dates,
                                     SpParams = SpParams, local_control = local_control, 
                                     PLCquantile = PLCquantile, qPLC_target = qPLC_target, qPLC_tol = qPLC_tol, max_rocks = max_rocks)
    parallel::stopCluster(cl)
  } else {
    if(progress) {
      cli::cli_progress_step(paste0("Rock optimization on ", n , " locations"))
      cli::cli_progress_bar(name = "Stands", total = n)
    }
    reslist <- vector("list", n)
    for(i in 1:n) {
      xi = list(i = i, 
                point = sf::st_geometry(sf)[i],
                forest = sf$forest[[i]], soil = sf$soil[[i]],
                meteo = meteolist[[i]],
                latitude = latitude[i], elevation = sf$elevation[i], slope= sf$slope[i], aspect = sf$aspect[i])
      reslist[[i]] <- .f_optim_rock(xi = xi, 
                           meteo = meteo, dates = dates, model = model,
                           SpParams = SpParams, local_control = local_control,
                           PLCquantile = PLCquantile, qPLC_target = qPLC_target, qPLC_tol = qPLC_tol, max_rocks = max_rocks)
      if(progress) cli::cli_progress_update()
    }
    if(progress) {
      cli::cli_progress_done()
    }
  }
  soillist <- vector("list", n)
  messages <- rep(NA, n)
  for(i in 1:n) {
    res_i <- reslist[[i]]
    messages[i] <- res_i$message
    newsoil <- sf$soil[[i]]
    if(messages[i]=="OK") {
      newsoil$rfc <- res_i$RFC
    }
    soillist[[i]] <- newsoil
  }
  sf$soil <- soillist
  sf$optimization_message <- messages
  return(sf::st_as_sf(tibble::as_tibble(sf)))
}