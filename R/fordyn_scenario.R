.standingVolume<-function(y, SpParams, volume_function){
  n = nrow(y)
  volume_spp = rep(0, nrow(SpParams))
  names(volume_spp) = SpParams$Name
  for(i in 1:n) {
    f = y$forest[[i]]
    if(!is.null(f)) {
      if(inherits(f, "forest")) {
        vols_i <- do.call(what = volume_function, 
                          args = list(x = f$treeData))*y$represented_area[i]
        nsp_i <- medfate::species_characterParameter(f$treeData$Species, SpParams, "Name")
        vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
        if(length(vol_sp_i)>0) volume_spp[names(vol_sp_i)] = volume_spp[names(vol_sp_i)] + vol_sp_i
      }
    }
  }
  return(volume_spp)
}

#' Scenario of forest dynamics
#' 
#' Evaluates forest dynamics over a landscape including climate and management scenarios
#'
#' @param sf An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link{soil}}.}
#'     \item{\code{state}: Objects of class \code{\link{spwbInput}} or \code{\link{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{management_unit}: Management unit corresponding to each stand.}
#'     \item{\code{represented_area}: Area represented by each stand (in hectares).}
#'   }
#'   Alternatively, the user may supply the result of a previous call to \code{fordyn_scenario}, where
#'   to continue simulations.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}). 
#' @param meteo Meteorology data (see \code{\link{fordyn_spatial}}).
#' @param local_control A list of local model control parameters (see \code{\link{defaultControl}}).
#' @param volume_function A function accepting a forest object or a tree data table as input and returning the wood volume (m3/ha) corresponding to each tree cohort.
#' If NULL, the default volume function is used (not recommended!).
#' @param management_scenario A list defining the management scenario (see \code{\link{create_management_scenario}})
#' @param dates A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param summary_function An appropriate function to calculate summaries from an object of class 'fordyn' (e.g., \code{\link{summary.fordyn}}).
#' @param summary_arguments List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' 
#' @details Management is implemented using the \code{\link{defaultManagementFunction}} in medfate, 
#' meaning that management parameters need to follow the structure of \code{\link{defaultManagementArguments}}
#' 
#' @returns An list of class 'fordyn_scenario' with the following elements:
#'  \itemize{
#'    \item{\code{result_sf}: An object of class 'sf' containing four elements:
#'      \itemize{
#'        \item{\code{geometry}: Spatial geometry.}
#'        \item{\code{id}: Stand id, taken from the input.}
#'        \item{\code{tree_table}: A list of data frames for each simulated stand, containing the living trees at each time step.}
#'        \item{\code{shrub_table}: A list of data frames for each simulated stand, containing the living shrub at each time step.}
#'        \item{\code{dead_tree_table}: A list of data frames for each simulated stand, containing the dead trees at each time step.}
#'        \item{\code{dead_shrub_table}: A list of data frames for each simulated stand, containing the dead shrub at each time step.}
#'        \item{\code{cut_tree_table}: A list of data frames for each simulated stand, containing the cut trees at each time step.}
#'        \item{\code{cut_shrub_table}: A list of data frames for each simulated stand, containing the cut shrub at each time step.}
#'        \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summary_function} was not \code{NULL}).}
#'      }
#'    }
#'    \item{\code{result_volumes}: A data frame with growth and extracted volumes (m3) by species and year. In demand-based scenarios target volumes are also included.}
#'    \item{\code{next_sf}: An object of class 'sf' to continue simulations in subsequent calls to \code{fordyn_scenario}.}
#'    \item{\code{next_demand}: In demand-based scenarios, a list with information (i.e. demand offset by species and last volume growth) 
#'    to modify demand in subsequent calls to \code{fordyn_scenario}.}
#'  }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{fordyn_spatial}}, \code{\link{create_management_scenario}}
#' 
#' @examples 
#' \dontrun{
#' # Load example landscape data
#' data("example_ifn")
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' #Prepare a three-year meteorological data in two blocks
#' meteo_01_02 <- rbind(examplemeteo, examplemeteo)
#' row.names(meteo_01_02) <- seq(as.Date("2001-01-01"), 
#'                               as.Date("2002-12-31"), by="day")
#' meteo_03 <- examplemeteo
#' row.names(meteo_03) <- seq(as.Date("2003-01-01"), 
#'                            as.Date("2003-12-31"), by="day")
#'                          
#' # Load default medfate parameters
#' data("SpParamsMED")
#' 
#' # Assign management unit to all stands
#' example_ifn$management_unit <- 1 
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' example_ifn$represented_area <- 100
#' 
#' # Creates scenario with one management unit and annual demand for P. nigra 
#' scen <- create_management_scenario(1, c("Pinus nigra" = 2300))
#' 
#' # Launch simulation scenario (years 2001 and 2002)
#' fs_12 <- fordyn_scenario(example_ifn, SpParamsMED, meteo = meteo_01_02, 
#'                        volume_function = NULL, management_scenario = scen,
#'                        parallelize = TRUE)
#'                        
#' # Continue simulation scenario 1 (year 2003)
#' fs_3 <- fordyn_scenario(fs_12, SpParamsMED, meteo = meteo_03, 
#'                        volume_function = NULL, management_scenario = scen,
#'                        parallelize = TRUE)
#'}
#'
fordyn_scenario<-function(sf, SpParams, meteo = NULL, 
                         management_scenario,
                         volume_function = NULL,
                         local_control = defaultControl(), dates = NULL,
                         CO2ByYear = numeric(0), summary_function=NULL, summary_arguments=NULL,
                         parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE){
  
  if(progress)  cli::cli_h1(paste0("Simulation of a management scenario with fordyn"))
  nspp = nrow(SpParams)
  
  offset_demand <- NULL
  last_growth <- NULL 
  if(inherits(sf, "sf")) {
    y <- sf
  } else if(inherits(sf, "fordyn_scenario")) {
    if(progress) cli::cli_progress_step(paste0("Recovering previous run"))
    y <- sf$next_sf
  } else {
    stop("Wrong class of input object. Should be an object of either class 'sf' or class 'fordyn_scenario'")
  }
  
  # A.1 Check inputs
  if(progress) cli::cli_progress_step(paste0("Checking sf input"))
  .check_sf_input(y)
  if(!("represented_area" %in% names(y))) stop("Column 'represented_area' must be defined in 'y'")
  if(!("management_unit" %in% names(y))) stop("Column 'management_unit' must be defined in 'y'")
  if(any(is.na(y$represented_area))) stop("Column 'represented_area' cannot include missing values")
  scenario_type = match.arg(management_scenario$scenario_type, c("bottom-up", "input_rate", "input_demand"))
  
  if(is.null(meteo) && !("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'sf' if not supplied separately")
  if(!is.null(meteo) && ("meteo" %in% names(y))) stop("Weather data supplied both as column 'meteo' and using parameter 'meteo'. Please choose one or the other")
  if("meteo" %in% names(y)) {
    if(progress) cli::cli_progress_step(paste0("Checking meteo column input"))
    meteo <- NULL
    y$meteo <- .check_meteo_column_input(y$meteo, dates) 
  } else {
    if(progress) cli::cli_progress_step(paste0("Checking meteo object input"))
    meteo <- .check_meteo_object_input(meteo, dates)
    y$meteo <- NULL
  }
  
  n = nrow(y)
  
  # If dates are not supplied, take them from weather input (already passed checks)
  if(is.null(dates)) {
    if(!is.null(meteo)) {
      if(inherits(meteo,"data.frame")) {
        if(!("dates" %in% names(meteo))) {
          datesMeteo <- as.Date(row.names(meteo))
        } else {
          datesMeteo <- as.Date(meteo$dates)
        }
      } else if(inherits(meteo, "stars")) {
        datesMeteo <- as.Date(stars::st_get_dimension_values(meteo, "date"))
      } else if(inherits(meteo, "list")) {
        datesMeteo <- NULL
        for(i in 1:length(meteo)) {
          dates_i<- as.Date(stars::st_get_dimension_values(meteo[[i]], "date"))
          if(is.null(datesMeteo)) datesMeteo <- dates_i
          else datesMeteo <- c(datesMeteo, dates_i)
        }
      }
    } else {
      if(!("dates" %in% names(y$meteo[[1]]))) {
        datesMeteo <- as.Date(row.names(y$meteo[[1]]))
      } else {
        datesMeteo <- as.Date(y$meteo[[1]]$dates)
      }
    }
    dates <- datesMeteo
  }
  if(progress) cli::cli_progress_done()
  
  # Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  
  # A.2 Scenario parameters
  if(progress) {
    cli::cli_h2("Scenario parameters")
    cli::cli_li(paste0("  Number of stands: ", n))
    cli::cli_li(paste0("  Number of years: ", length(years)))
    cli::cli_li(paste0("  Management scenario type: ", scenario_type))
  }
  if(scenario_type != "bottom-up"){
    spp_demand <- management_scenario$annual_demand_by_species
    target_spp_names <- names(spp_demand)
    offset_demand <- rep(0, length(spp_demand))
    names(offset_demand) <- target_spp_names
    if(inherits(sf, "fordyn_scenario")) {
      if("next_demand" %in% names(sf)) {
        offset <- sf$next_demand$offset
        offset_demand[names(offset)] <- offset
        last_growth <- sf$next_demand$last_growth
      }
    }
    if(scenario_type == "input_demand"){
      if(progress) {
        cli::cli_li(paste0("Fixed demand:\n"))
        if(is.vector(spp_demand)) {
          for(i in 1:length(spp_demand)) {
            if(spp_demand[i]>0) cat(paste0("     ", names(spp_demand)[i], " ", round(spp_demand[i],1), " m3/yr \n"))
          }
        }
      }
    }
    if(scenario_type == "input_rate"){
      extraction_rates <- management_scenario$extraction_rate_by_year
      if(!all(as.character(years) %in% names(extraction_rates))) stop("Extraction rates have not been specified for all simulation years")
      extraction_rates <- extraction_rates[as.character(years)]
      if(progress) {
        cli::cli_li(paste0("Input demand:\n"))
        if(is.vector(spp_demand)) {
          for(i in 1:length(spp_demand)) {
            if(spp_demand[i]>0) cat(paste0("      ", names(spp_demand)[i], " ", round(spp_demand[i],1), " m3/yr\n"))
          }
        }
        cli::cli_li(paste0("Extraction rates:\n"))
        for(i in 1:length(extraction_rates)) {
          cat(paste0("      yr. ", names(extraction_rates)[i], " - ", extraction_rates[i], "%\n"))
        }
      }
    }
  } 
  # Initialize missing management arguments according to unit
  if(!("management_arguments" %in% names(y))) y$management_arguments = vector("list", n)
  for(i in 1:n) {
    if(!is.na(y$management_unit[i])) {
      if(is.null(y$management_arguments[[i]])) y$management_arguments[[i]] = as.list(management_scenario$units[y$management_unit[i],])
    }
  }
  managed = !is.na(y$management_unit)
  units = sort(unique(y$management_unit), na.last = TRUE)
  if(progress) {
    cli::cli_li(paste0("Management units:"))
    for(i in 1:length(units)) {
      if(!is.na(units[i])) cat(paste0("     ", sum(y$management_unit==units[i], na.rm=TRUE), 
                                      " stands in unit [", row.names(management_scenario$units)[units[i]],"]\n"))
      else cat(paste0("     ", sum(is.na(y$management_unit)), " stands without prescriptions\n"))
    }
  }
  if(is.null(volume_function)) {
    if(progress) cli::cli_li("Default volume function")
    volume_function = "default_volume_function"
  } else {
    if(progress) cli::cli_li("User-defined volume function")
  }

  
  
  # Define summary function (to save memory)
  table_selection<-function(object, summary_function, summary_arguments, ...) {
    l = object[c("TreeTable", "ShrubTable",
                 "DeadTreeTable", "DeadShrubTable", 
                 "CutTreeTable", "CutShrubTable")]
    if(!is.null(summary_function)) {
      argList = list(object=object)
      if(!is.null(summary_arguments)) argList = c(argList, summary_arguments)
      l[["summary"]] = do.call(summary_function, args=argList)
    }
    return(l)
  }

  
  extracted = matrix(0, nspp, length(years))
  rownames(extracted) <- SpParams$Name
  growth <- extracted
  target <- extracted
  cumulative_extraction <- 0
  cumulative_growth <- 0
  cumulative_extraction_target <- 0
  cumulative_growth_target <- 0
  summary_list = vector("list", n)
  
  # Estimate initial standing volume
  if(progress) cli::cli_li(paste0("Initial volume calculation"))
  initial_volume_spp <- .standingVolume(y, SpParams, volume_function)

  # B. Year loop
  if(progress) cli::cli_h2("Simulation")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cli::cli_h3(paste0(" [ Year ", year, " (", yi,"/", length(years),") ]"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    # Set demand in demand-based scenarios
    if(scenario_type=="input_demand") {
      spp_demand_year <- spp_demand + offset_demand
      target[names(spp_demand_year), yi] = spp_demand_year
    } else if(scenario_type=="input_rate") {
      extraction_rate_year <- extraction_rates[as.character(years[yi])]
      if(!is.null(last_growth)) {
        total_extraction_year = last_growth*(extraction_rate_year/100)
        if(progress) cli::cli_li(paste0("  Previous growth ", round(last_growth), " m3 ","   target extraction rate: ", extraction_rate_year, "%","   target volume (no offset): ", round(total_extraction_year), " m3"))
        spp_demand_year <- as.numeric(total_extraction_year)*(spp_demand/sum(spp_demand))
      } else {
        if(progress) cli::cli_li(paste0("  Extraction rate: ", extraction_rate_year, "% cannot be applied (previous growth is missing)"))
        spp_demand_year <- spp_demand
      }
      spp_demand_year <- spp_demand_year + offset_demand
      target[names(spp_demand_year),yi] = spp_demand_year
    }

    # B.1 Determine which plots will be managed according to current demand
    managed_step <- managed # by default, manage all plots that have
    if(scenario_type != "bottom-up") {
      if(progress) {
        if(any(spp_demand_year != 0)) {
          cli::cli_li(paste0("  Species with demand for current year:"))
          for(i in 1:length(spp_demand_year)) {
            if(spp_demand_year[i] != 0) cat(paste0("      ", names(spp_demand_year)[i], " ", round(spp_demand_year[i]), " m3\n"))
          }
        } else {
          cli::cli_li(paste0("  No species with demand"))
        }
      }
      vol_spp_target <- matrix(0, n, length(target_spp_names)) 
      colnames(vol_spp_target) <- target_spp_names
      rownames(vol_spp_target) <- y$id
      final_cuts <- rep(FALSE, n)
      # print(sum(unlist(lapply(y$management_arguments, FUN = is.null))))
      for(i in 1:n) {
        if(managed[i]) {
          man_args <- y$management_arguments[[i]] 
          f <- y$forest[[i]]
          if(nrow(f$treeData)>0) {
            if((man_args$type=="regular") && (man_args$finalPreviousStage > 0)) final_cuts[i] = TRUE
            man <- do.call(what = "defaultManagementFunction", 
                           args = list(x = f, args=man_args))
            ctd <- f$treeData
            ctd$N <- man$N_tree_cut
            vols_i <- do.call(what = volume_function, args = list(x = ctd))*y$represented_area[i]
            nsp_i <- medfate::plant_speciesName(f, SpParams)[1:nrow(f$treeData)]
            vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
            vol_sp_i <- vol_sp_i[names(vol_sp_i) %in% target_spp_names]
            if(length(vol_sp_i)>0) vol_spp_target[i, names(vol_sp_i)] = vol_sp_i
            # DECREASE TARGET according to FINAL CUTS
            if(final_cuts[i]) spp_demand_year = spp_demand_year - vol_spp_target[i,, drop=FALSE]
          }
        }
      }
      # print(data.frame(vol_spp_target = rowSums(vol_spp_target), final_cut = final_cuts))
      # Set all plots not in final cuts to non-management
      no_final <- which(!final_cuts)
      managed_step[no_final] <- FALSE 
      to_cut <- integer(0)
      # For each species set to true
      for(j in 1:length(spp_demand_year)) {
        vol_spp_j <- vol_spp_target[,j]
        if(spp_demand_year[j]>0) {
          # cat(paste0("Species ", names(spp_demand_year)[j], " Target ", spp_demand_year[j],"\n"))
          o <- order(vol_spp_j, decreasing = TRUE)
          vol_cum_sorted_j <- cumsum(sort(vol_spp_j, decreasing = TRUE))
          vol_cum_j <- vol_spp_j
          vol_cum_j[o] <- vol_cum_sorted_j
          sel_cut_j <- (vol_cum_j < spp_demand_year[j])
          sel_cut_j[o[1]] = TRUE # Set plot with highest volume value to TRUE
          sel_cut_j[vol_spp_j==0] = FALSE # Set plots without volume to FALSE
          # print(cbind(vol_spp_j, vol_cum_j, sel_cut_j))
          managed_step[no_final[sel_cut_j]] = TRUE # Set selected plots to TRUE
        }
      }
      if(progress) {
        cli::cli_li(paste0(sum(managed_step), " stands to be managed (", sum(final_cuts) , " because of final cuts):"))
        for(i in 1:length(units)) {
          if(!is.na(units[i])) {
            n_man_unit <- sum(y$management_unit[managed_step]==units[i], na.rm=TRUE)
            if(n_man_unit>0) cat(paste0("      ", n_man_unit , 
                                          " stands in unit [", row.names(management_scenario$units)[units[i]],"]\n"))
          }          
        }
      }
    } else {
      if(progress) cli::cli_li(paste0(sum(managed_step), " stands allowed to be managed"))
    }
    prev_management_args <- y$management_arguments
    y$management_arguments[managed & (!managed_step)] <- list(NULL) # Deactivates management on plots that were not selected
    
    # B.2 Call fordyn_spatial()
    if(progress) cli::cli_li(paste0("Calling fordyn_spatial..."))
    fds <-.model_spatial(y, SpParams, meteo = meteo, model = "fordyn", local_control = local_control, dates = datesYear,
                        management_function = "defaultManagementFunction",
                        CO2ByYear = CO2ByYear, keep_results = FALSE, summary_function=table_selection, 
                        summary_arguments=list(summary_function = summary_function, summary_arguments = summary_arguments),
                        parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, 
                        progress = progress)

    # B.3 Update final state variables in y and retrieve fordyn tables
    # y_backup <- rlang::duplicate(y)
    y_backup <- y
    y <- update_landscape(y, fds) # This updates forest, soil, growthInput and management_arguments
    # B.3b Check for null forest objects (from errors in previous step)
    restored <- 0
    for(i in 1:n) {
      if(inherits(fds$result[[i]], "error")) {
        print(fds$result[[i]])
        forest_backup <- y_backup$forest[[i]]
        y$forest[[i]] <- forest_backup
        if("state" %in% names(y_backup)) {
          state_backup <- y_backup$state[[i]]
          if(!is.null(state_backup)) y$state[[i]] <- state_backup
        } 
        if("soil" %in% names(y_backup)) {
          y$soil[[i]] <- y_backup$soil[[i]]
          if(inherits(y$soil[[i]], "data.frame")) y$soil[[i]] <- soil(y$soil[[i]])
        }
        if(is.null(y$state[[i]])) y$state[[i]] <- forest2growthInput(y$forest[[i]], y$soil[[i]], SpParams, local_control)
        
        if("management_arguments" %in% names(y_backup)) {
          man_backup  <- y_backup$management_arguments[[i]]
          if(!is.null(man_backup)) y$management_arguments[[i]] <- man_backup
          else y$management_arguments[[i]] <- list(NULL)
        }
        restored <- restored +1
      }
    }
    if(progress && (restored>0)) cli::cli_li(paste0("Restored ", restored, " stands with errors during simulation to last year state"))
    
    # For those plots that were not managed but have prescriptions in a demand-based scenario, 
    # copy back management arguments and increase the variable years since thinning
    y$management_arguments[managed & (!managed_step)] <- prev_management_args[managed & (!managed_step)]
    for(i in 1:n){
      if(managed[i] && (!managed_step[i])){
        if(!is.na(y$management_arguments[[i]]$yearsSinceThinning)) y$management_arguments[[i]]$yearsSinceThinning = y$management_arguments[[i]]$yearsSinceThinning + 1
      }
    }
    # Move summary into results
    fds$result <- fds$summary 
    # Retrieve user-defined summaries (if existing)
    if(!is.null(summary_function)) {
      for(i in 1:n){
        summary_i <- fds$result[[i]]$summary
        if(!is.null(summary_i)) {
          if(yi==1) summary_list[[i]] <- summary_i
          else summary_list[[i]] <- rbind(summary_list[[i]], summary_i)
        }
      }
    }
    # Retrieve tree tables
    if(yi==1) {
      tree_table <- .fordyn_tables(fds, "TreeTable")
      shrub_table <- .fordyn_tables(fds, "ShrubTable")
      dead_tree_table <- .fordyn_tables(fds, "DeadTreeTable")
      dead_shrub_table <- .fordyn_tables(fds, "DeadShrubTable")
      cut_tree_table <- .fordyn_tables(fds, "CutTreeTable")
      cut_shrub_table <- .fordyn_tables(fds, "CutShrubTable")
    } else {
      tt_i <- .fordyn_tables(fds, "TreeTable")
      tt_i <- tt_i[tt_i$Step==1,, drop = FALSE]
      tt_i$Step <- yi
      tree_table <- dplyr::bind_rows(tree_table, tt_i)
      st_i <- .fordyn_tables(fds, "ShrubTable")
      st_i <- st_i[st_i$Step==1,]
      st_i$Step <- yi
      shrub_table <- dplyr::bind_rows(shrub_table, st_i)
      dtt_i <- .fordyn_tables(fds, "DeadTreeTable")
      dtt_i$Step <- yi
      dead_tree_table <- dplyr::bind_rows(dead_tree_table, dtt_i)
      dst_i <- .fordyn_tables(fds, "DeadShrubTable")
      dst_i$Step <- yi
      dead_shrub_table <- dplyr::bind_rows(dead_shrub_table, dst_i)
      ctt_i <- .fordyn_tables(fds, "CutTreeTable")
      ctt_i$Step <- yi
      cut_tree_table <- dplyr::bind_rows(cut_tree_table, ctt_i)
      cst_i <- .fordyn_tables(fds, "CutShrubTable")
      cst_i$Step <- yi
      cut_shrub_table <- dplyr::bind_rows(cut_shrub_table, cst_i)
    }
    # B.4 Store actual extraction
    for(i in 1:n){
      ctt <- fds$result[[i]]$CutTreeTable
      if(!is.null(ctt)) {
        vols_i <- do.call(what = volume_function, 
                          args = list(x = ctt))*y$represented_area[i]
        nsp_i <- medfate::species_characterParameter(ctt$Species, SpParams, "Name")
        vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
        extracted[names(vol_sp_i), yi] <- extracted[names(vol_sp_i), yi] + vol_sp_i
      }
    }
    # B.5 Update extraction rates and actual satisfied demand
    if(progress) cli::cli_li(paste0("Final volume calculation"))
    final_volume_spp <- .standingVolume(y, SpParams, volume_function)
    growth[,yi] <- final_volume_spp - initial_volume_spp + extracted[,yi]
    extracted_sum <- sum(extracted[,yi], na.rm=TRUE)
    initial_sum <- sum(initial_volume_spp, na.rm = TRUE)
    final_sum <- sum(final_volume_spp, na.rm = TRUE)
    growth_sum <- sum(growth[,yi], na.rm=TRUE)
    cumulative_extraction <- cumulative_extraction + extracted_sum
    cumulative_growth <- cumulative_growth + growth_sum
    if(scenario_type != "bottom-up") {
      extracted_target_sum <- sum(extracted[target_spp_names, yi], na.rm=TRUE)
      initial_target_sum <- sum(initial_volume_spp[target_spp_names], na.rm = TRUE)
      final_target_sum <- sum(final_volume_spp[target_spp_names], na.rm = TRUE)
      growth_target_sum <- sum(growth[target_spp_names,yi], na.rm=TRUE)
      cumulative_extraction_target <- cumulative_extraction_target + extracted_target_sum
      cumulative_growth_target <- cumulative_growth_target + growth_target_sum
    }
    if(progress) {
      cli::cli_li(paste0("Management statistics:"))
      cat(paste0("      Initial (m3): ", round(initial_sum),
                 "      Growth (m3): ", round(growth_sum), 
                 "      Extraction (m3): ", round(extracted_sum), 
                 "      Final (m3): ", round(final_sum), 
                 "\n"))
      cat(paste0("      Extraction rate: ", round(100*extracted_sum/max(0,growth_sum)),"%"))
      if(yi>1) cat(paste0("      Average extraction rate: ", round(100*cumulative_extraction/max(0,cumulative_growth)),"%\n"))
      else cat("\n")
      if(scenario_type != "bottom-up") {
        cli::cli_li(paste0("Management statistics (target species):"))
        cat(paste0("      Initial (m3): ", round(initial_target_sum),
                   "      Growth (m3): ", round(growth_target_sum), 
                   "      Extraction (m3): ", round(extracted_target_sum), 
                   "      Final (m3): ", round(final_target_sum), 
                   "\n"))
        target_sum = sum(target[,yi])
        cat(paste0("      Target volume (m3): ", round(target_sum), 
                   "      Target satisfaction: ", round(100*extracted_target_sum/target_sum), "%",
                   "\n"))
        cat(paste0("      Extraction rate: ", round(100*extracted_sum/max(0,growth_target_sum)),"%"))
        if(yi>1) cat(paste0("      Average extraction rate: ", round(100*cumulative_extraction_target/max(0,cumulative_growth_target)),"%\n"))
      }
    }

    # recalculate offset and previous growth for next year (or next simulation)
    if(scenario_type != "bottom-up") {
      offset_demand <- target[target_spp_names, yi] - extracted[target_spp_names,yi]
      last_growth <- growth_target_sum
    }
    # Initial standing volume for next year
    initial_volume_spp <- final_volume_spp
  }
  if(progress) cli::cli_h2("Arranging output")
  if(progress) cli::cli_li(" Tree/shrub tables")
  tree_tables <- vector("list", n)
  shrub_tables <- vector("list", n)
  dead_tree_tables <- vector("list", n)
  dead_shrub_tables <- vector("list", n)
  cut_tree_tables <- vector("list", n)
  cut_shrub_tables <- vector("list", n)
  for(i in 1:n) {
    tree_tables[[i]] <- tree_table[tree_table$id==y$id[i], ]
    shrub_tables[[i]] <- shrub_table[shrub_table$id==y$id[i], ]
    dead_tree_tables[[i]] <- dead_tree_table[dead_tree_table$id==y$id[i], ]
    dead_shrub_tables[[i]] <- dead_shrub_table[dead_shrub_table$id==y$id[i], ]
    cut_tree_tables[[i]] <- cut_tree_table[cut_tree_table$id==y$id[i], ]
    cut_shrub_tables[[i]] <- cut_shrub_table[cut_shrub_table$id==y$id[i], ]
  }
  sf_results <- sf::st_sf(geometry=sf::st_geometry(y))
  sf_results$id <- y$id
  sf_results$tree_table <- tree_tables
  sf_results$shrub_table <- shrub_tables
  sf_results$dead_tree_table <- dead_tree_tables
  sf_results$dead_shrub_table <- dead_shrub_tables
  sf_results$cut_tree_table <- cut_tree_tables
  sf_results$cut_shrub_table <- cut_shrub_tables
  sf_results$summary <- summary_list
  
  # Volumes extracted
  if(progress) cli::cli_li(" Wood volume table")
  extracted <- data.frame(Name = SpParams$Name, extracted)
  names(extracted) <- c("species", as.character(years))
  row.names(extracted) <- NULL
  target <- data.frame(Name = SpParams$Name, target)
  names(target) <- c("species", as.character(years))
  row.names(target) <- NULL
  growth <- data.frame(Name = SpParams$Name, growth)
  names(growth) <- c("species", as.character(years))
  row.names(growth) <- NULL
  extracted_pv <- tidyr::pivot_longer(extracted, as.character(years), names_to="year", values_to = "extracted")
  target_pv <- tidyr::pivot_longer(target, as.character(years), names_to="year", values_to = "target")
  growth_pv <- tidyr::pivot_longer(growth, as.character(years), names_to="year", values_to = "growth")
  if(management_scenario$scenario_type != "bottom-up") {
    volumes <- growth_pv |>
      dplyr::full_join(target_pv, by=c("species", "year"))|>
      dplyr::full_join(extracted_pv, by=c("species", "year")) |>
      dplyr::filter(growth!=0 | target!=0 | extracted!=0)
  } else {
    volumes <- growth_pv |>
      dplyr::full_join(extracted_pv, by=c("species", "year")) |>
      dplyr::filter(growth!=0 | extracted!=0) 
  }
  l <- list(result_sf = sf::st_as_sf(tibble::as_tibble(sf_results)),
           result_volumes = volumes,
           next_sf = y)
  if(scenario_type != "bottom-up") {
    l[["next_demand"]] <- list(offset = offset_demand, last_growth = last_growth)
  }
  class(l)<-c("fordyn_scenario", "list")
  return(l)
}
