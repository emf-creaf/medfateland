.f_spatial<-function(xi, meteo, dates, model,
                     SpParams, local_control, 
                     CO2ByYear = numeric(0),
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
  # If burn_doy_by_year is specified reset column FireProbability and fill appropriate values. For each year:
  #   burn_doy_by_year == NA : No burning
  #   burn_doy_by_year == 0 : Set the driest day (i.e. with lowest VPD) to burn
  #   burn_doy_by_year > 0 & burn_doy_by_year < 366 : Set the indicated doy to burn
  if(model=="fordyn" && (!is.null(xi$burn_doy_by_year))) {
    met$FireProbability <- 0.0
    if(!("DOY" %in% names(met))) {
      met$DOY <- as.numeric(format.Date(met$dates, "%j"))
    }
    met$Year <- format.Date(met$dates, "%Y")
    met$VPD <- NA
    for(id in 1:nrow(met)) {
      vp <- meteoland::utils_averageDailyVP(Tmin = met$MinTemperature[id], Tmax = met$MaxTemperature[id],
                                            RHmin = met$MinRelativeHumidity[id], RHmax = met$MaxRelativeHumidity[id])
      met$VPD[id] <- max(0, meteoland::utils_saturationVP(met$MaxTemperature[id]) - vp)
    }
    for(iy in 1:length(xi$burn_doy_by_year)) {
      year <- names(xi$burn_doy_by_year)[iy]
      if(year %in% unique(met$Year)) {
        doy <- xi$burn_doy_by_year[iy]
        if(!is.na(doy)) {
          if(doy == 0.0) {
            year_days <- which(met$Year==year)
            max_VPD <- which.max(met$VPD[year_days])
            burn_day <- year_days[max_VPD]
          } else {
            burn_day <- which((met$Year==year) & (met$DOY == doy))
          }
          if(length(burn_day)!=1) stop("Wrong fire regime specification")
          met$FireProbability[burn_day] <- 1.0
        }
      }
    }
    met$VPD <- NULL
    met$DOY <- NULL
    met$Year <- NULL
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
        medfate::aspwb(x, meteo=met,
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
        medfate::aspwb(x, meteo=met,
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
        # print("entering fordyn")
        medfate::fordyn(forest = f_in, soil = s, SpParams = SpParams, meteo=met, control = local_control,
                        latitude = xi$latitude, elevation = xi$elevation,
                        slope = xi$slope, aspect = xi$aspect,
                        CO2ByYear = CO2ByYear,
                        management_function = mf, management_args = ma)
        # print("after fordyn")
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
.check_meteo_column_input <- function(meteo_column, dates = NULL, check_equal_dates = FALSE) {
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
    } else if(check_equal_dates) {
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
                        CO2ByYear = numeric(0), fire_regime = NULL,
                        keep_results = TRUE,
                        management_function = NULL, 
                        summary_function=NULL, summary_arguments=NULL,
                        parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL,
                        progress = TRUE, local_verbose = FALSE) {
  
  if(progress) cli::cli_progress_step(paste0("Checking sf input"))
  .check_sf_input(y)
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]

  n <- nrow(y)
  local_control$verbose <- local_verbose
  
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
    meteolist <- .check_meteo_column_input(y$meteo, dates, FALSE) 
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
        local_control_i <- NULL
        if("local_control" %in% names(y)) {
          if(!is.null(y$local_control[[i]])) {
            if(inherits(y$local_control[[i]], "list")) local_control_i <- y$local_control[[i]]
          }
        }
        if(is.null(local_control_i)) local_control_i <- local_control
        local_control_i$verbose <- local_verbose
        
        if(inherits(s, "data.frame")) {
          s <- soil(s)
        }
        if(landcover[i] == "wildland") {
          f = forestlist[[i]]
          if(inherits(f, "forest") && inherits(s, "soil")) {
            if(model=="spwb") {
              xlist[[i]] = medfate::spwbInput(f, s, SpParams, local_control_i)
            } else if(model=="growth") {
              xlist[[i]] = medfate::growthInput(f, s, SpParams, local_control_i)
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
      if(progress) cli::cli_alert_info(paste0("All input objects are already available for '", model, "'"))
    }
  } 

  burn_doy_by_year_list <- vector("list", n)
  if(!is.null(fire_regime)) {
    if(inherits(fire_regime, "fire_regime")) {
      if(progress) {
        cli::cli_progress_step("Generating fire regime instance")
      }
      fire_instance_matrix <- fire_regime_instance(y, fire_regime)
    } else if(inherits(fire_regime, "matrix")) {
      if(progress) {
        cli::cli_progress_step("Checking fire regime instance")
      }
      if(nrow(fire_regime)!=n) stop("Fire regime instance matrix should have the same number of rows as 'sf'")
      if(!all(rownames(fire_regime)== y$id)) stop("Fire regime row names should be equal to values in 'id' column of 'sf'")
      fire_instance_matrix <- fire_regime
    } else {
      stop("Wrong class for 'fire_regime'. Has to be of class 'fire_regime' or 'matrix'")
    }
    for(i in 1:n) {
      b <- as.vector(fire_instance_matrix[i, ])
      names(b) <- colnames(fire_instance_matrix)
      burn_doy_by_year_list[[i]] <- b
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
                     burn_doy_by_year = burn_doy_by_year_list[[i]],
                     latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                     management_args = managementlist[[i]])
    }
    if(progress) cli::cli_progress_step(paste0("Launching parallel computation (cores = ", num_cores, "; chunk size = ", chunk_size,")"))
    cl<-parallel::makeCluster(num_cores)
    reslist_parallel <- parallel::parLapplyLB(cl, XI, .f_spatial, 
                                             meteo = meteo, dates = dates, model = model,
                                             SpParams = SpParams, local_control = local_control, 
                                             CO2ByYear = CO2ByYear,
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
                burn_doy_by_year = burn_doy_by_year_list[[i]],
                latitude = latitude[i], elevation = y$elevation[i], slope= y$slope[i], aspect = y$aspect[i],
                management_args = managementlist[[i]])
      sim_out = .f_spatial(xi = xi, 
                           meteo = meteo, dates = dates, model = model,
                           SpParams = SpParams, local_control = local_control, 
                           CO2ByYear = CO2ByYear,
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
#' Functions that allow calling local models \code{\link[medfate]{spwb}}, \code{\link[medfate]{growth}} or \code{\link[medfate]{fordyn}}, for a set of forest stands distributed in specific locations. 
#' No spatial processes are simulated.
#' 
#' @param sf An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland' or 'agriculture').}
#'     \item{\code{forest}: Objects of class \code{\link[medfate]{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link[medfate]{soil}} or data frames of physical properties.}
#'     \item{\code{state}: Objects of class \code{\link[medfate]{spwbInput}} or \code{\link[medfate]{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
#'     \item{\code{local_control}: A list of control parameters (optional). Used to override function parameter \code{local_control} for specific locations (values can be \code{NULL} for the remaining ones).}
#'     \item{\code{management_arguments}: Lists with management arguments. Optional, relevant for \code{fordyn_spatial} only.}
#'     \item{\code{represented_area_ha}: Area represented by each stand in hectares. Optional, relevant for \code{fordyn_spatial} when 
#'     \code{fire_regime} is supplied only).}
#'     \item{\code{ignition_weights}: Relative weights to determine stands to be burned. Optional, relevant for \code{fordyn_spatial} when 
#'     \code{fire_regime} is supplied only).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param meteo Input meteorological data (see section details). If NULL, the function will expect a column 'meteo' in parameter \code{y}.
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}}) for function \code{\link[medfate]{spwb_day}} or \code{\link[medfate]{growth_day}}.
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param fire_regime A list of parameters defining the fire regime (see \code{\link{create_fire_regime}}) or 
#'                    a matrix representing a fire regime instance (see \code{\link{fire_regime_instance}}), 
#'                    to be used in simulations with \code{\link{fordyn_spatial}}. If NULL, wildfires are not simulated. 
#' @param keep_results Boolean flag to indicate that point/cell simulation results are to be returned (set to \code{FALSE} and use summary functions for large data sets).
#' @param summary_function An appropriate function to calculate summaries (e.g., \code{\link[medfate]{summary.spwb}}).
#' @param summary_arguments List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information of simulations.
#' @param local_verbose Boolean flag to display detailed progress information in local simulations.
#' @param management_function A function that implements forest management actions (see \code{\link[medfate]{fordyn}}).
#' of such lists, one per spatial unit.
#' 
#' @details Simulation functions  accept different formats for meteorological input (parameter \code{meteo}). 
#' The user may supply two kinds of daily weather sources: 
#' \enumerate{
#'   \item{A data frame with meteorological data common for all spatial location (spatial variation of weather not considered).}
#'   \item{An object or (a list of objects) of class \code{stars} with reference interpolation data created by package \code{\link[meteoland]{meteoland}}.
#'         If a list of such \emph{interpolator} objects is supplied, the simulation functions will interpolate on the target locations for the periods covered by each interpolator, 
#'         but the user will be responsible for supplying interpolators in the correct temporal order.}
#' }
#' Alternatively, the user may leave parameter \code{meteo = NULL} and specify a weather data frame for each element of \code{y}
#' in a column named 'meteo'.
#' 
#' Fire regimes are only allowed for function \code{fordyn_spatial}. If an object of class \code{fire_regime} is supplied, the function will call
#' \code{\link{fire_regime_instance}} to generate a realization of the fire regime before conducting simulations. Alternatively,
#' users can directly supply a fire regime instance matrix, derived from another source (e.g. a fire landscape model). Note that operating
#' with fire regimes assumes all forest stands share the same period of simulation, but enforcing this is left to the user.
#' 
#' @returns An object of class 'sf' containing four elements:
#' \itemize{
#'   \item{\code{geometry}: Spatial geometry.}
#'   \item{\code{id}: Stand id, taken from the input.}
#'   \item{\code{state}: A list of \code{\link[medfate]{spwbInput}} or \code{\link[medfate]{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}) or with NULL values whenever simulation errors occurred.}
#'   \item{\code{forest}: A list of \code{\link[medfate]{forest}} objects for each simulated stand (only in function \code{fordyn_spatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}) or with NULL values whenever simulation errors occurred.}
#'   \item{\code{management_arguments}: A list of management arguments for each simulated stand (only in function \code{fordyn_spatial} if management function was supplied), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'   \item{\code{result}: A list of model output for each simulated stand. Some elements can contain an error condition if the simulation resulted in an error. Values will be NULL (or errors) if \code{keep_results = FALSE}.}
#'   \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summary_function} was not \code{NULL}), with NULL values whenever simulation errors occurred.}
#' }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link[medfate]{spwb}}, \code{\link[medfate]{growth}}, \code{\link[medfate]{fordyn}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{simulation_summary}} , \code{\link{plot_summary}}, 
#' \code{\link{initialize_landscape}}, \code{\link{update_landscape}}
#' 
#' @examples
#' \donttest{
#' # Load example landscape data
#' data("example_ifn")
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Subset two plots to speed-up calculations
#' example_subset <- example_ifn[31:32, ]
#' 
#' # Perform simulation
#' dates <- seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
#' res <- spwb_spatial(example_subset, SpParamsMED, examplemeteo, dates = dates)
#'   
#' # Perform fordyn simulation for one year (one stand) without management
#' res_noman <- fordyn_spatial(example_subset, SpParamsMED, examplemeteo)
#' 
#' }
#' 
#' @name spwb_spatial
#' @export
spwb_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                     CO2ByYear = numeric(0), keep_results = TRUE, summary_function=NULL, summary_arguments=NULL,
                     parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE,
                     local_verbose = FALSE) {
  if(progress)  cli::cli_h1(paste0("Simulation of model 'spwb'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "spwb", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress,
                local_verbose = local_verbose)
}

#' @rdname spwb_spatial
#' @export
growth_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), fire_regime = NULL,
                       keep_results = TRUE, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE,
                       local_verbose = FALSE) {
  if(progress)  cli::cli_h1(paste0("Simulation of model 'growth'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "growth", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, fire_regime = fire_regime, keep_results = keep_results, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress,
                local_verbose = local_verbose)
}

#' @rdname spwb_spatial
#' @export
fordyn_spatial<-function(sf, SpParams, meteo = NULL, local_control = defaultControl(), dates = NULL,
                       CO2ByYear = numeric(0), fire_regime = NULL, 
                       keep_results = TRUE, 
                       management_function = NULL, summary_function=NULL, summary_arguments=NULL,
                       parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE,
                       local_verbose = FALSE) {
  if(progress)  cli::cli_h1(paste0("Simulation of model 'fordyn'"))
  .model_spatial(y=sf, SpParams = SpParams, meteo = meteo, model = "fordyn", local_control = local_control, dates = dates,
                CO2ByYear = CO2ByYear, fire_regime = fire_regime, keep_results = keep_results, 
                management_function = management_function, summary_function = summary_function, summary_arguments = summary_arguments, 
                parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, progress = progress,
                local_verbose = local_verbose)
}
