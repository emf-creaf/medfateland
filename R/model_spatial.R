.f_spatial<-function(xi, meteo, dates, model,
                     SpParams, local_control, CO2ByYear = numeric(0), 
                     keep_results = TRUE,
                     management_function = NULL, summary_function = NULL, summary_arguments = NULL){
  f <- xi$forest
  s <- xi$soil
  x <- xi$x

  f_out <- NULL
  m_out <- NULL
  x_out <- NULL
  res <- NULL
  summary <- NULL
  
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
  if(model=="spwb") {
    if(inherits(x, "spwbInput")){
      res <- tryCatch({
          medfate::spwb(x, meteo=met,
                         latitude = xi$latitude, elevation = xi$elevation,
                         slope = xi$slope, aspect = xi$aspect,
                         CO2ByYear = CO2ByYear)
        }, error = function(e) {
          simpleError(e$message,"spwb")
        })
    } else if(inherits(x, "aspwbInput")){
      res <- tryCatch({
          .aspwb(x, meteo=met,
                 latitude = xi$latitude, elevation = xi$elevation,
                 slope = xi$slope, aspect = xi$aspect)
      }, error = function(e) {
        simpleError(e$message,"aspwb")
      })
    } 
  } else if(model=="growth") {
    if(inherits(x, "growthInput")) {
      res <-tryCatch({
        medfate::growth(x, meteo=met,
                        latitude = xi$latitude, elevation = xi$elevation,
                        slope = xi$slope, aspect = xi$aspect,
                        CO2ByYear = CO2ByYear)
      },
      error = function(e){
        simpleError(e$message,"growth")
      })
    } else if(inherits(x, "aspwbInput")){
      res <- tryCatch({
        .aspwb(x, meteo=met,
               latitude = xi$latitude, elevation = xi$elevation,
               slope = xi$slope, aspect = xi$aspect)
      }, error = function(e) {
        simpleError(e$message,"aspwb")
      })
    } 
  } else if(model=="fordyn") {
    if(inherits(s, "data.frame")) {
      s <- soil(s)
    }
    if(inherits(f, "forest") && inherits(s, "soil")) {
      mf <- management_function
      ma <- xi$management_args
      if(!is.null(x)) { #If there is a previous state, combine it with f so that state variables are no reinitialized
        f_in <- list(NextForestObject = f,
                     NextInputObject = x)
        class(f_in) <- c("fordyn", "list")
      } else { # Otherwise use the input object (this implies initialization of state variables)
        f_in <- f
      }
      if(is.null(ma)) mf = NULL
      res <- tryCatch({
        medfate::fordyn(forest = f_in, soil = s, SpParams = SpParams, meteo=met, control = local_control,
                        latitude = xi$latitude, elevation = xi$elevation,
                        slope = xi$slope, aspect = xi$aspect,
                        CO2ByYear = CO2ByYear,
                        management_function = mf, management_args = ma)
      },
      error = function(e){
        simpleError(e$message,"fordyn")
      })
    }
  } 
  if(!is.null(res) && !inherits(res, "error")) {
    if(!is.null(summary_function)){
      argList <- list(object=res)
      if(!is.null(summary_arguments)) argList = c(argList, summary_arguments)
      summary <- do.call(summary_function, args=argList)
    }
    if(model=="fordyn"){
      fs_i <- res$ForestStructures
      f_out <- res$NextForestObject
      m_out <- res$ManagementArgs
    }
    if(model=="spwb") x_out <- res$spwbOutput
    else if(model=="growth") x_out <- res$growthOutput
    else if(model=="fordyn") x_out <- res$NextInputObject
    # Frees memory if detailed results are not required
    if(!keep_results) res <- NULL
  }
  return(list(result = res, summary = summary, x_out = x_out, forest_out = f_out, management_out = m_out))
}

.check_meteo_object_input <- function(meteo, dates = NULL) {
  if(!is.null(dates)) {
    if(inherits(meteo, "data.frame")) {
      if("dates" %in% names(meteo)) {
        datesMeteo <- as.Date(meteo$dates)
      } else {
        datesMeteo <- as.Date(row.names(meteo))
      }
      if(sum(datesMeteo %in% dates) < length(dates)) {
        stop("Supplied weather data frame does not cover all elements in 'dates'")
      }
      # Subset dates in meteo
      if("dates" %in% names(meteo)) {
        meteo <- meteo[meteo$dates %in% dates, , drop = FALSE]
      } else {
        meteo <- meteo[as.character(dates), , drop = FALSE]
      }
    }
    if(inherits(meteo, "stars")) {
      datesMeteo <- as.Date(stars::st_get_dimension_values(meteo, "date"))
      if(sum(datesMeteo %in% dates) < length(dates)) {
        stop("Supplied weather interpolator does not cover all elements in 'dates'")
      }
      # Subset dates in meteo
      params <- get_interpolation_params(meteo)
      meteo <- meteo[,which(datesMeteo %in% dates),] |>
        set_interpolation_params(params)
    } else if(inherits(meteo, "list")) {
      meteo_red <- list()
      datesMeteo <- NULL
      drop  <- rep(FALSE, length(meteo))
      for(i in 1:length(meteo)) {
        meteo_i <- meteo[[i]]
        dates_i<- as.Date(stars::st_get_dimension_values(meteo_i, "date"))
        if(is.null(datesMeteo)) datesMeteo <- dates_i
        else datesMeteo <- c(datesMeteo, dates_i)
        # Subset dates in meteo_i
        params_i <- get_interpolation_params(meteo_i)
        w_i <- which(dates_i %in% dates)
        if(length(w_i)>0) {
          meteo_red_i <- meteo_i[,w_i,] |>
            set_interpolation_params(params_i)
          meteo[[i]] <- meteo_red_i
        } else {
          drop[i] <- TRUE
        }
      }
      meteo <- meteo[!drop]
      if(sum(datesMeteo %in% dates) < length(dates)) {
        stop("Supplied weather interpolator(s) do not cover all elements in 'dates'")
      }
    }
  }
  return(meteo)
}
.check_meteo_column_input <- function(meteo_column, dates = NULL) {
  datesMeteo_1 <- NULL
  for(i in 1:length(meteo_column)) {
    met_i <- meteo_column[[i]]
    if(is.null(met_i)) stop(paste0("NULL wheather in row ",i))
    if(!inherits(met_i, "data.frame")) stop(paste0("Weather data of row ", i, " does not inherit 'data.frame' class"))
    if("dates" %in% names(met_i)) {
      tryCatch({
        datesMeteo_i <- as.Date(met_i$dates)
      },  error = function(e) {stop(paste0("Could not retrieve dates from column 'dates' in row ", i))})
    } else {
      tryCatch({
        datesMeteo_i <- as.Date(row.names(met_i))
      },  error = function(e) {stop(paste0("Could not retrieve dates from row.names in row ", i))})
    }
    if(i==1) datesMeteo_1 <- datesMeteo_i
    if(!is.null(dates)) {
      if(sum(datesMeteo_i %in% dates) < length(dates)) {
        stop(paste0("Supplied weather data frame for row ",i," does not cover all elements in 'dates'"))
      }
      # Subset dates in met_i
      if("dates" %in% names(met_i)) {
        met_i <- met_i[met_i$dates %in% dates, , drop = FALSE]
      } else {
        met_i <- met_i[as.character(dates), , drop = FALSE]
      }
      meteo_column[[i]] <- met_i
    } else {
      # check that all items have same dates
      if(!all(datesMeteo_i==datesMeteo_1)) warning(paste0("Weather data of row ", i, " does not have the same dates as row 1."))
    }
  }
  return(meteo_column)
}

.check_soil_column_input<- function(soil_column, land_cover_type = NULL) {
  if(is.null(land_cover_type)) land_cover_type <- rep("wildland", length(soil_column)) # If not supplied assumes wildland
  for(i in 1:length(soil_column)) {
    s <- soil_column[[i]]
    if(land_cover_type[i] %in% c("wildland", "agriculture")) {
      if(is.null(s)) stop(paste0("NULL soil data in row ", i, " for land cover '", land_cover_type[i], "'"))
      if(!inherits(s, c("soil", "data.frame")))  stop(paste0("Soil data of row ", i," is not a 'soil' or 'data.frame' object"))
    }
  }
}

.check_sf_input<-function(y) {
  if(!inherits(y, "sf")) stop("'sf' has to be an object of class 'sf'.")
  if(!all(c("elevation","slope","aspect") %in% names(y))) stop("Columns 'elevation', 'slope' and 'aspect' must be defined.")
  if(!("soil" %in% names(y))) stop("Column 'soil' must be defined.")
  check_forest <- TRUE
  if("land_cover_type" %in% names(y)) {
    if(!inherits(y$land_cover_type, "character")) stop("Column 'land_cover_type' should be a character vector.")
    if(sum(y$land_cover_type=="agriculture")>0) {
      if(!("crop_factor" %in% names(y))) stop("Column 'crop_factor' must be defined to simulate soil water balance in agricultural locations.")
      if(!inherits(y$crop_factor, "numeric")) stop("Column 'crop_factor' should be a numeric vector.")
      if(sum(y$land_cover_type=="agriculture")==nrow(y)) check_forest <- FALSE
    }
    .check_soil_column_input(y$soil, y$land_cover_type)
  } else {
    .check_soil_column_input(y$soil)
  }
  if(check_forest && !("forest" %in% names(y))) stop("Column 'forest' must be defined.")
}

.model_spatial<-function(y, SpParams, meteo = NULL, model = "spwb",
                        local_control = defaultControl(), dates = NULL,
                        CO2ByYear = numeric(0), keep_results = TRUE,
                        management_function = NULL, 
                        summary_function=NULL, summary_arguments=NULL,
                        parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL,
                        progress = TRUE) {
  
  if(progress) cli::cli_progress_step(paste0("Checking sf input"))
  .check_sf_input(y)
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]

  local_control$verbose = FALSE

  n <- nrow(y)
  
  if("forest" %in% names(y)) {
    forestlist <- y$forest
  } else {
    forestlist <- vector("list", n)
  }
  if("soil" %in% names(y)) {
    soillist <- y$soil
  } else {
    soillist <- vector("list", n)
  }
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
    xlist  = y$state
  } else {
    xlist = vector("list",n)
  }
  if("management_arguments" %in% names(y)) {
    managementlist  = y$management_arguments
  } else {
    managementlist = vector("list",n)
  }
  if(is.null(meteo) && !("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'y' if not supplied separately")
  if("meteo" %in% names(y)) {
    if(progress) cli::cli_progress_step(paste0("Checking meteo column input"))
    meteo <- NULL
    meteolist <- .check_meteo_column_input(y$meteo, dates) 
  } else {
    if(progress) cli::cli_progress_step(paste0("Checking meteo object input"))
    meteo <- .check_meteo_object_input(meteo, dates)
    meteolist <- vector("list",n)
  }
  
  resultlist = vector("list",n)
  summarylist = vector("list",n)
  forestlist_out = vector("list",n)
  managementlist_out = vector("list",n)

  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      if(landcover[i] == "wildland") {
        f = forestlist[[i]]
        s = soillist[[i]]
        if(inherits(f, "forest") && inherits(s, c("soil", "data.frame"))) {
          init[i] = TRUE
          x = xlist[[i]]
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
        cli::cli_progress_step(paste0("Creating ", length(w_init), " input objects for model '", model, "'"))
      }
      for(w in 1:length(w_init)) {
        i = w_init[w]
        s = soillist[[i]]
        if(inherits(s, "data.frame")) {
          s <- soil(s)
        }
        if(landcover[i] == "wildland") {
          f = forestlist[[i]]
          if(inherits(f, "forest") && inherits(s, "soil")) {
            if(model=="spwb") {
              xlist[[i]] = medfate::forest2spwbInput(f, s, SpParams, local_control)
            } else if(model=="growth") {
              xlist[[i]] = medfate::forest2growthInput(f, s, SpParams, local_control)
            }
          }
        } else if(landcover[i] == "agriculture") {
          xlist[[i]] <- .aspwbInput(crop_factor = cropfactor[i], control = local_control, soil = s)
        }
      }
      if(progress) {
        cli::cli_progress_done()
      }
    } else {
      if(progress) cli::cli_alert_info(paste0("All input objects are already available for '", model, "'"))
    }
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
                     id = y$id[i], 
                     point = sf::st_geometry(y)[i],
                     forest = forestlist[[i]], soil = soillist[[i]], x = xlist[[i]],
                     meteo = meteolist[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                     management_args = managementlist[[i]])
    }
    if(progress) cli::cli_progress_step(paste0("Launching parallel computation (cores = ", num_cores, "; chunk size = ", chunk_size,")"))
    cl<-parallel::makeCluster(num_cores)
    reslist_parallel <- parallel::parLapplyLB(cl, XI, .f_spatial, 
                                             meteo = meteo, dates = dates, model = model,
                                             SpParams = SpParams, local_control = local_control, CO2ByYear = CO2ByYear,
                                             keep_results = keep_results,
                                             management_function = management_function, 
                                             summary_function = summary_function, summary_arguments = summary_arguments,
                                             chunk.size = chunk_size)
    parallel::stopCluster(cl)
    if(progress) {
      cli::cli_progress_step("Retrieval of results")
    }
    for(i in 1:n) {
      if(!is.null(reslist_parallel[[i]]$x_out)) xlist[[i]] = reslist_parallel[[i]]$x_out
      if(!is.null(reslist_parallel[[i]]$result)) resultlist[[i]] = reslist_parallel[[i]]$result
      if(!is.null(reslist_parallel[[i]]$summary)) summarylist[[i]] = reslist_parallel[[i]]$summary
      if(model=="fordyn"){
        if(!is.null(reslist_parallel[[i]]$forest_out)) forestlist_out[[i]] = reslist_parallel[[i]]$forest_out
        if(!is.null(reslist_parallel[[i]]$management_out)) managementlist_out[[i]] = reslist_parallel[[i]]$management_out
      }
    }
    if(progress) {
      cli::cli_progress_done()
    }
  } else {
    if(progress) {
      cli::cli_progress_step(paste0("Performing '", model, "' simulations on ", n , " locations"))
      cli::cli_progress_bar(name = "Stands", total = n)
     }
     for(i in 1:n) {
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
      if(progress) cli::cli_progress_update()
    }
    if(progress) {
      cli::cli_progress_done()
    }
  }
  res = sf::st_sf(geometry=sf::st_geometry(y))
  res$id <- y$id
  res$state <- xlist
  if(model=="fordyn") {
    res$forest <- forestlist_out
    if(!is.null(management_function)) res$management_arguments <- managementlist_out
  }
  res$result <- resultlist
  errors <- sapply(res$result, function(x){inherits(x, "error")})
  if(sum(errors)>0) {
    cli::cli_alert_warning(paste0("Simulation errors occurred in ", sum(errors), " out of ",n ," stands. Check error messages in 'result'"))
  } else {
    if(progress) cli::cli_alert_success(paste0("No simulation errors detected"))
  }
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
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland' or 'agriculture').}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link{soil}} or data frames of physical properties.}
#'     \item{\code{state}: Objects of class \code{\link{spwbInput}} or \code{\link{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
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
#' The user may supply two kinds of daily weather sources: 
#' \enumerate{
#'   \item{A data frame with meteorological data common for all spatial location (spatial variation of weather not considered).}
#'   \item{An object or (a list of objects) of class \code{\link{stars}} with reference interpolation data created by package \code{\link{meteoland}}.
#'         If a list of such \emph{interpolator} objects is supplied, the simulation functions will interpolate on the target locations for the periods covered by each interpolator, 
#'         but the user will be responsible for supplying interpolators in the correct temporal order.}
#' }
#' Alternatively, the user may leave parameter \code{meteo = NULL} and specify a weather data frame for each element of \code{y}
#' in a column named 'meteo'.
#' 
#' @returns An object of class 'sf' containing four elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{id}: Stand id, taken from the input.}
#'   \item{\code{state}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}) or with NULL values whenever simulation errors occurred.}
#'   \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in function \code{fordyn_spatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}) or with NULL values whenever simulation errors occurred.}
#'   \item{\code{management_arguments}: A list of management arguments for each simulated stand (only in function \code{fordyn_spatial} if management function was supplied), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'   \item{\code{result}: A list of model output for each simulated stand. Some elements can contain an error condition if the simulation resulted in an error. Values will be NULL (or errors) if \code{keep_results = FALSE}.}
#'   \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summary_function} was not \code{NULL}), with NULL values whenever simulation errors occurred.}
#' }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link{spwb}}, \code{\link{growth}}, \code{\link{fordyn}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{simulation_summary}} , \code{\link{plot_summary}}, 
#' \code{\link{initialize_landscape}}, \code{\link{update_landscape}}
#' 
#' @examples
#' \dontrun{
#' # Load example landscape data
#' data("example_ifn")
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Perform simulation
#' dates <- seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
#' res <- spwb_spatial(example_ifn, SpParamsMED, examplemeteo, dates = dates)
#'   
#' # Generate summaries (these could have also been specified when calling 'spwbspatial')
#' res_sum <- simulation_summary(res, summary_function = summary.spwb, freq="month")
#' 
#' # Plot summaries
#' plot_summary(res_sum, "Transpiration", "2001-03-01")
#' 
#' # Fordyn simulation for one year (one stand) without management
#' res_noman <- fordyn_spatial(example_ifn[1,], SpParamsMED, examplemeteo)
#' 
#' # Add management arguments to all stands
#' example_ifn$management_arguments <- vector("list", nrow(example_ifn))
#' for(i in 1:nrow(example_ifn)) example_ifn$management_arguments[[i]] <- defaultManagementArguments()
#' 
#' # Change thinning threshold for stand #1
#' example_ifn$management_arguments[[1]]$thinningThreshold <- 15
#' 
#' # Fordyn simulation for one year (one stand) with management
#' res_man <- fordyn_spatial(example_ifn[1,], SpParamsMED, examplemeteo,
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
  if(progress)  cli::cli_h1(paste0("Simulation of model 'spwb'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "spwb", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}

#' @rdname spwb_spatial
growth_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), keep_results = TRUE, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  if(progress)  cli::cli_h1(paste0("Simulation of model 'growth'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "growth", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}

#' @rdname spwb_spatial
fordyn_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), keep_results = TRUE, 
                       management_function = NULL, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE) {
  if(progress)  cli::cli_h1(paste0("Simulation of model 'fordyn'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "fordyn", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, 
                management_function = management_function, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress)
}

#' Initialization of model inputs for spatially-distributed forest stands
#' 
#' Initializes state for local models \code{\link{spwb}} or \code{\link{growth}}. 
#' 
#' @param x An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link{soil}} or data frames of physical properties.}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland' or 'agriculture').}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param local_control A list of control parameters (see \code{\link{defaultControl}}).
#' @param model A string to indicate the model, either \code{"spwb"} or \code{"growth"}.
#' @param replace Boolean flag to replace existing initialized states
#' @param progress Boolean flag to display progress information.
#' 
#' @returns Replaces or adds column 'state' whose elements are \code{\link{spwbInput}} or \code{\link{growthInput}} objects 
#' and returns the modified object of class 'sf'.
#' 
#' @details
#' Initialization is dealt automatically when calling simulation functions \code{\link{spwb_spatial}},  \code{\link{growth_spatial}},
#' \code{\link{spwb_spatial_day}} or \code{\link{growth_spatial_day}}. However, function \code{initialize_landscape}  allows separating initialization from model simulations.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link{spwb_spatial}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{update_landscape}}
#' 
#' @examples
#' # Load example landscape data
#' data("example_ifn")
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Initialize state for 'spwb' simulations
#' example_ifn <- initialize_landscape(example_ifn, SpParamsMED, 
#'                                     defaultControl(), model = "spwb")
#' 
#' @name initialize_landscape
initialize_landscape<- function(x, SpParams, local_control, model = "spwb", replace = FALSE, progress = TRUE) {
  match.arg(model, c("spwb", "growth"))
  if(!inherits(x, "sf")) stop("'x' has to be an object of class 'sf'.")
  if(!("forest" %in% names(x))) stop("Column 'forest' must be defined.")
  if(!("soil" %in% names(x))) stop("Column 'soil' must be defined.")
  forestlist = x$forest
  soillist  = x$soil
  n <- length(forestlist)
  if("state" %in% names(x)) {
    xlist  = x$state
  } else {
    xlist = vector("list",n)
  }
  if("land_cover_type" %in% names(x)) {
    landcover <- x$land_cover_type
  } else {
    landcover <- rep("wildland", n)
  }
  if("crop_factor" %in% names(x)) {
    cropfactor <- x$crop_factor
  } else {
    cropfactor <- rep(NA, n)
  }
  n = length(forestlist)
  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      if(landcover[i] == "wildland") {
        f = forestlist[[i]]
        s = soillist[[i]]
        if(inherits(f, "forest") && inherits(s, c("soil", "data.frame"))) {
          init[i] = TRUE
          x_i = xlist[[i]]
          if(!replace) {
            if(inherits(x_i,"spwbInput") && model=="spwb") init[i] = FALSE
            if(inherits(x_i,"growthInput") && model=="growth") init[i] = FALSE
          }
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
        cli::cli_progress_step(paste0("Creating ", length(w_init), " input objects for model '", model, "'."))
        cli::cli_progress_bar(name = "Stands", total = n)
      }
      for(w in 1:length(w_init)) {
        i = w_init[w]
        s = soillist[[i]]
        if(inherits(s, "data.frame")) {
          s <- soil(s)
        }
        if(landcover[i] == "wildland") {
          f = forestlist[[i]]
          if(inherits(f, "forest") && inherits(s, "soil")) {
            if(model=="spwb") {
              xlist[[i]] = medfate::forest2spwbInput(f, s, SpParams, local_control)
            } else if(model=="growth") {
              xlist[[i]] = medfate::forest2growthInput(f, s, SpParams, local_control)
            }
          }
        } else if(landcover[i] == "agriculture") {
          xlist[[i]] <- .aspwbInput(crop_factor = cropfactor[i], control = local_control, soil = s)
        }
        if(progress) cli::cli_progress_update()
      }
      if(progress) cli::cli_progress_done()
    } else {
      if(progress) cli::cli_alert_info(paste0("All input objects are already available for '", model, "'. Use 'replace = TRUE' to force replacing current inputs."))
    }
  } 
  x[["state"]] <- xlist
  return(x)
}