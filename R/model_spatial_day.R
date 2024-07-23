.f_spatial_day<-function(xi, meteo, date, model){
  res = NULL
  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) {
      if(!("dates" %in% names(meteo))) {
        meteovec <- unlist(meteo[date, ]) 
      } else {
        meteovec <- unlist(meteo[as.character(meteo$dates)==as.character(date), ])
      }
    }
    else if(inherits(meteo, "stars")) {
      pt_sf <- sf::st_sf(geometry = xi$point, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect)
      met <- meteoland::interpolate_data(pt_sf, meteo, dates = as.Date(date), 
                                         verbose = FALSE, ignore_convex_hull_check = TRUE)
      met <- met$interpolated_data[[1]]
      meteovec <- unlist(met[as.character(met$dates)==as.character(date), ])
    }    
  } else { # Weather in 'meteo' element
    if(!("dates" %in% names(xi$meteo))) {
      meteovec <- unlist(xi$meteo[date, ]) 
    } else {
      meteovec <- unlist(xi$meteo[as.character(xi$meteo$dates)==as.character(date), ])
    }
  }
  
  if(model=="spwb") {
    if(inherits(xi$x, "spwbInput")){
      res<-medfate::spwb_day(xi$x, date, meteovec,
                             latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                             modifyInput = TRUE)
    } else if(inherits(xi$x, "aspwbInput")) {
      res <- medfate::aspwb_day(xi$x, date, meteovec,
                       latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                       modifyInput = TRUE)
    }
  } else if(model=="growth") {
    if(inherits(xi$x, "growthInput")) {
      res<-medfate::growth_day(xi$x, date, meteovec,
                               latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                               modifyInput = TRUE)
    } else if(inherits(xi$x, "aspwbInput")) {
      res <- medfate::aspwb_day(xi$x, date, meteovec,
                        latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                        modifyInput = TRUE)
      
    }
  } 
  return(res)
}

.model_spatial_day<-function(y, meteo, date, model = "spwb", 
                            SpParams, 
                            local_control = defaultControl(),
                            parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL,
                            progress = TRUE) {
  
  
  if(progress)  cli::cli_h1(paste0("Simulation of model '",model, "' for date '", date,"'"))
  
  if(progress) cli::cli_progress_step(paste0("Checking sf input"))
  .check_sf_input(y)
  
  latitude <- sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]
  date <- as.character(date)
  
  local_control$verbose <- FALSE
  
  n <- nrow(y)
  forestlist <- y$forest
  soillist  <- y$soil
  if("land_cover_type" %in% names(y)) {
    landcover <- y$land_cover_type
  } else {
    landcover <- rep("wildland", n)
  }
  if("crop_factor" %in% names(y)) {
    cropfactor <- y$crop_factor
  } else {
    cropfactor <- rep(NA, n)
  }
  if("state" %in% names(y)) {
    xlist  <- y$state
  } else {
    xlist <- vector("list", n)
  }
  
  resultlist = vector("list",n)
  summarylist = vector("list",n)
  names(resultlist) = names(forestlist)
  names(summarylist) = names(forestlist)
  
  if(is.null(meteo) && !("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'y' if not supplied separately")
  if("meteo" %in% names(y)) {
    if(progress) cli::cli_progress_step(paste0("Checking meteo column input"))
    meteo <- NULL
    meteolist <- .check_meteo_column_input(y$meteo, as.Date(date), FALSE) 
  } else {
    if(progress) cli::cli_progress_step(paste0("Checking meteo object input"))
    meteo <- .check_meteo_object_input(meteo, as.Date(date))
    meteolist <- vector("list",n)
  }
  
  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      if(landcover[i] == "wildland") {
        f <- forestlist[[i]]
        s <- soillist[[i]]
        if(inherits(f, "forest") && inherits(s, c("soil","data.frame"))) {
          init[i] <- TRUE
          x <- xlist[[i]]
          if(inherits(x,"spwbInput") && model=="spwb") init[i] = FALSE
          if(inherits(x,"growthInput") && model=="growth") init[i] = FALSE
        }
      } else if(landcover[i] == "agriculture") {
        if(inherits(soillist[[i]], c("soil","data.frame"))) {
          init[i] <- TRUE
        }
      }
    }
    w_init = which(init)
    if(length(w_init)>0) {
      if(progress) { 
        cli::cli_progress_step(paste0("Creating '", length(w_init), "' input objects."))
      }
      for(w in 1:length(w_init)) {
        i <- w_init[w]
        s <- soillist[[i]]
        local_control_i <- NULL
        if("local_control" %in% names(y)) {
          if(!is.null(y$local_control[[i]])) {
            if(inherits(y$local_control[[i]], "list")) local_control_i <- y$local_control[[i]]
          }
        }
        if(is.null(local_control_i)) local_control_i <- local_control
        if(inherits(s, "data.frame")){
          s <- soil(s)
        }
        if(landcover[i] == "wildland") {
          f <- forestlist[[i]]
          if(inherits(f, "forest") && inherits(s, "soil")) {
            if(model=="spwb") {
              xlist[[i]] <- medfate::spwbInput(f, s, SpParams, local_control_i)
            } else if(model=="growth") {
              xlist[[i]] <- medfate::growthInput(f, s, SpParams, local_control_i)
            }
          }
        } else if(landcover[i] == "agriculture") {
          xlist[[i]] <- medfate::aspwbInput(crop_factor = cropfactor[i], control = local_control_i, soil = s)
        }
      }
      if(progress) {
        cli::cli_progress_done()
      }
    } else {
      if(progress) cli::cli_alert_info(paste0("All input objects are already available for '", model, "'."))
    }
  }


  if(parallelize) {
    if(progress) {
      cli::cli_progress_step("Preparing data for parellization.")
    }
    
    if(is.null(chunk_size)) chunk_size = floor(n/num_cores)
    
    XI = vector("list", n)
    for(i in 1:n) {
      XI[[i]] = list(i = i, 
                     id = y$id[i], 
                     point = sf::st_geometry(y)[i], x = xlist[[i]],
                     meteo = meteolist[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i])
    }
    if(progress) cli::cli_progress_step(paste0("Launching parallel computation (cores = ", num_cores, "; chunk size = ", chunk_size,")."))
    cl<-parallel::makeCluster(num_cores)
    reslist_parallel = parallel::parLapplyLB(cl, XI, .f_spatial_day, 
                                             meteo = meteo, date = date, model = model,
                                             chunk.size = chunk_size)
    parallel::stopCluster(cl)
    if(progress) {
      cli::cli_progress_step("Retrieval of results.")
    }
    for(i in 1:n) {
      resultlist[[i]] = reslist_parallel[[i]]
    }
    if(progress) {
      cli::cli_progress_done()
    }
  } else {
    if(progress) {
      cli::cli_li(paste0("Performing '", model, "' simulations."))
      cli::cli_progress_bar(name = "Stands", total = n)
    }
    for(i in 1:n) {
      xi = list(i = i, id = y$id[i],
                point = sf::st_geometry(y)[i], x = xlist[[i]],
                meteo = meteolist[[i]],
                latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i])
      resultlist[[i]] = .f_spatial_day(xi, meteo = meteo, date = date, model = model)
      if(progress) cli::cli_progress_update()
    }
  }
  res <- sf::st_sf(geometry=sf::st_geometry(y))
  res$id <- y$id
  res$state <- xlist
  res$result <- resultlist
  return(sf::st_as_sf(tibble::as_tibble(res)))
}

#' One-day simulation for spatially-distributed forest stands
#' 
#' Functions that allow calling local models \code{\link[medfate]{spwb_day}} or \code{\link[medfate]{growth_day}}, 
#' for a set of forest stands distributed in specific locations and a given date. 
#' No spatial processes are simulated.
#'
#' @param sf An object of class \code{\link[sf]{sf}} with landscape information (see \code{\link{spwb_spatial}}).
#' @param meteo Meteorology data (see \code{\link{spwb_spatial}}).
#' @param date A string with the date to be simulated.
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param local_control A list of local model control parameters (see \code{\link[medfate]{defaultControl}}).
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' 
#' @returns An object of class \code{\link[sf]{sf}} the same name as the function called containing three elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{id}: Stand id, taken from the input.}
#'   \item{\code{state}: A list of model input objects for each simulated stand, to be used in subsequent simulations.}
#'   \item{\code{result}: A list of model output for each simulated stand.}
#' }
#' 
#' @details Simulation functions accept different formats for meteorological input (described in \code{\link{spwb_spatial}}).
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link[medfate]{spwb_day}}, \code{\link[medfate]{growth_day}}, \code{\link{spwb_spatial}}
#' 
#' @examples 
#' \donttest{
#' #Load example landscape data
#' data("example_ifn")
#'
#' #Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' #Load default medfate parameters
#' data("SpParamsMED")
#'   
#' #Perform simulation
#' date <- "2001-03-01"
#' res <- spwb_spatial_day(example_ifn, examplemeteo, date, SpParamsMED)
#' }
#' @name spwb_spatial_day
#' @export
spwb_spatial_day<-function(sf, meteo = NULL, date, SpParams, local_control = defaultControl(),
                         parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  .model_spatial_day(y=sf, meteo = meteo, date = date, model = "spwb", SpParams = SpParams, local_control = local_control, 
                    parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}
#' @rdname spwb_spatial_day
#' @export
growth_spatial_day<-function(sf, meteo = NULL, date, SpParams, local_control = defaultControl(),
                           parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  .model_spatial_day(y=sf, meteo = meteo, date = date, model = "growth", SpParams = SpParams, local_control = local_control,
                    parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}
