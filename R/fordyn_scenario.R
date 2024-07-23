.standingVolume<-function(y, SpParams, volume_function, volume_arguments){
  n = nrow(y)
  volume_spp = rep(0, nrow(SpParams))
  names(volume_spp) = SpParams$Name
  for(i in 1:n) {
    f = y$forest[[i]]
    if(!is.null(f)) {
      if(inherits(f, "forest")) {
        argList <- list(x = f$treeData, SpParams = SpParams)
        if(!is.null(volume_arguments)) argList = c(argList, volume_arguments)
        vols_i <- do.call(what = volume_function, 
                          args = argList)*y$represented_area_ha[i]
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
#' @param sf An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Stand identifiers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{forest}: Objects of class \code{\link[medfate]{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link[medfate]{soil}}.}
#'     \item{\code{state}: Objects of class \code{\link[medfate]{spwbInput}} or \code{\link[medfate]{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{management_unit}: Management unit corresponding to each stand.}
#'     \item{\code{represented_area_ha}: Area represented by each stand in hectares.}
#'     \item{\code{ignition_weights}: Relative weights to determine stands to be burned. Optional, relevant when 
#'                 \code{fire_regime} is supplied only).}
#'     \item{\code{local_control}: A list of control parameters (optional). Used to override function parameter \code{local_control} for specific stands (values can be \code{NULL} for the remaining ones).}
#'   }
#'   Alternatively, the user may supply the result of a previous call to \code{fordyn_scenario}, where
#'   to continue simulations.
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}). 
#' @param meteo Meteorology data (see \code{\link{fordyn_spatial}}).
#' @param local_control A list of local model control parameters (see \code{\link[medfate]{defaultControl}}).
#' @param volume_function A function accepting a forest object or a tree data table, and a species parameter table, as input and 
#' returning the wood volume (m3/ha) corresponding to each tree cohort. The function may accept additional arguments.
#' If NULL, the default volume function is used (not recommended!).
#' @param volume_arguments List with additional arguments for the volume function.
#' @param management_scenario A list defining the management scenario (see \code{\link{create_management_scenario}})
#' @param dispersal_control A list of dispersal control parameters (see \code{\link{default_dispersal_control}}). If NULL, then dispersal is not simulated. 
#' @param dates A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param fire_regime A list of parameters defining the fire regime (see \code{\link{create_fire_regime}}) or 
#'                    a matrix representing a fire regime instance (see \code{\link{fire_regime_instance}}). 
#'                    If NULL, wildfires are not simulated. Details are given in \code{\link{fordyn_spatial}}.
#' @param summary_function An appropriate function to calculate summaries from an object of class 'fordyn' (e.g., \code{\link[medfate]{summary.fordyn}}).
#' @param summary_arguments List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param num_cores Integer with the number of cores to be used for parallel computation.
#' @param chunk_size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' @param verbose Boolean flag to display additional console output.
#' 
#' @details This function allows coordinating the dynamics of simulated forest stands via a management scenario 
#' defined at the landscape/regional level (see different kinds of scenarios and how to specify them in \code{\link{create_management_scenario}}).
#' 
#' The input 'sf' object has to be in a Universal Transverse Mercator (UTM) coordinate system (or any other projection using meters as length unit)
#' for appropriate behavior of dispersal sub-model.
#'
#' For each year to be simulated, the function determines which forest stands will be managed, possibly depending on the demand,
#' and then calls function \code{\link{fordyn_spatial}} for one year (normally including parallelization). 
#' If the simulation of some stands results in an error, the function will try to restore 
#' the previous state of the forest stand for the next year steps. Finally, the function evaluates how much of the specified demand
#' has been fulfilled and stores the results, including demand offsets to be applied the year after.
#' 
#' Management is implemented using the \code{\link[medfate]{defaultManagementFunction}} in medfate, 
#' meaning that management parameters need to follow the structure of \code{\link[medfate]{defaultManagementArguments}}
#' 
#' Details about the inclusion of fire regimes in simulations are explained in \code{\link{fordyn_spatial}}.
#'  
#' @returns An list of class 'fordyn_scenario' with the following elements:
#'  \itemize{
#'    \item{\code{result_sf}: An object of class 'sf' using a UTM projection and containing four elements:
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
#'    \item{\code{result_volumes}: A data frame with initial, growth, extracted and final volumes (m3) by year. In demand-based scenarios volumes corresponding to species with demand are also included.}
#'    \item{\code{result_volumes_spp}: A data frame with growth and extracted volumes (m3) by species and year.}
#'    \item{\code{result_volumes_demand}: In demand-based scenarios target volumes are also included, a data frame with growth, target and extracted volumes (m3) by demand entity and year. .}
#'    \item{\code{next_sf}: An object of class 'sf' to continue simulations in subsequent calls to \code{fordyn_scenario}.}
#'    \item{\code{next_demand}: In demand-based scenarios, a list with information (i.e. demand offset by species and last volume growth) 
#'    to modify demand in subsequent calls to \code{fordyn_scenario}.}
#'  }
#' 
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Aitor \enc{Améztegui}{Ameztegui}, UdL
#' 
#' @seealso \code{\link{fordyn_spatial}}, \code{\link{create_management_scenario}}, \code{\link{dispersal}}
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
#' # Creates scenario with one management unit and annual demand for P. nigra 
#' scen <- create_management_scenario(1, c("Pinus nigra/Pinus sylvestris" = 2300))
#' 
#' # Assign management unit to all stands
#' example_ifn$management_unit <- 1 
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' example_ifn$represented_area_ha <- 100
#' 
#' # Transform to UTM31 (necessary for dispersal)
#' example_ifn_utm31 <- sf::st_transform(example_ifn, crs = 32631)
#' 
#' # Subset three plots to speed-up calculations
#' example_subset <- example_ifn_utm31[31:33, ]
#' 
#' # Launch simulation scenario
#' fs_12 <- fordyn_scenario(example_subset, SpParamsMED, meteo = examplemeteo, 
#'                          volume_function = NULL, management_scenario = scen,
#'                          parallelize = FALSE)
#' }
#' @export
fordyn_scenario<-function(sf, SpParams, meteo = NULL, 
                         management_scenario, 
                         volume_function = NULL, volume_arguments = NULL,
                         local_control = defaultControl(), 
                         dispersal_control = default_dispersal_control(),
                         dates = NULL,
                         CO2ByYear = numeric(0), fire_regime = NULL,
                         summary_function=NULL, summary_arguments=NULL,
                         parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, 
                         progress = TRUE, verbose = FALSE){
  
  if(progress)  cli::cli_h1(paste0("Simulation of a management/fire scenario with fordyn"))
  nspp = nrow(SpParams)
  
  # Disable seed bank dynamics inside fordyn (it is dealt with in dispersal)
  local_control$allowSeedBankDynamics <- is.null(dispersal_control)
    
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
  if(!("represented_area_ha" %in% names(y))) stop("Column 'represented_area_ha' must be defined in 'y'")
  if(!("management_unit" %in% names(y))) stop("Column 'management_unit' must be defined in 'y'")
  if(any(is.na(y$represented_area_ha))) stop("Column 'represented_area_ha' cannot include missing values")
  scenario_type = match.arg(management_scenario$scenario_type, c("bottom-up", "input_rate", "input_demand"))
  
  if(is.null(meteo) && !("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'sf' if not supplied separately")
  if(!is.null(meteo) && ("meteo" %in% names(y))) stop("Weather data supplied both as column 'meteo' and using parameter 'meteo'. Please choose one or the other")
  if("meteo" %in% names(y)) {
    if(progress) cli::cli_progress_step(paste0("Checking meteo column input"))
    meteo <- NULL
    y$meteo <- .check_meteo_column_input(y$meteo, dates, TRUE) 
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
    cli::cli_li(paste0("  Represented area: ", round(sum(y$represented_area_ha)), " ha"))
    cli::cli_li(paste0("  Number of years: ", length(years)))
    cli::cli_li(paste0("  Management scenario type: ", scenario_type))
  }
  if(scenario_type != "bottom-up"){
    spp_demand <- management_scenario$annual_demand_by_species
    if(!is.numeric(spp_demand)) stop("Annual demand by species must be a named numeric vector")
    target_taxon_names <- names(spp_demand) # Species or species groups
    if(is.null(target_taxon_names)) stop("Annual demand should be named")
    target_split_names <- strsplit(target_taxon_names, "/") # List of species included in each item
    target_spp_names <- unlist(target_split_names) # Vector of species names mentioned
    if(length(target_spp_names) > length(unique(target_spp_names))) {
      stop("Demand species names cannot be repeated!")
    }
    if(!all(target_spp_names %in% SpParams$Name)) stop("Some demand names do not match species names of 'SpParams'")
    offset_demand <- rep(0, length(spp_demand)) # Same vector length as annual demand
    names(offset_demand) <- target_taxon_names
    if(inherits(sf, "fordyn_scenario")) {
      if("next_demand" %in% names(sf)) {
        offset <- sf$next_demand$offset
        if(sum(names(offset) %in% target_taxon_names)< length(offset))  stop("Offset names do not all match to taxon names in demand")
        offset_demand[names(offset)] <- offset
        last_growth <- sf$next_demand$last_growth
      }
    }
    if(scenario_type == "input_demand"){
      if(verbose) {
        cli::cli_li(paste0("Fixed demand:\n"))
        if(is.vector(spp_demand)) {
          for(i in 1:length(spp_demand)) {
            name_spp_demand_i <- names(spp_demand)[i]
            if(nchar(name_spp_demand_i)> 30) name_spp_demand_i <- paste0(substr(name_spp_demand_i,1,27),"...")
            if(spp_demand[i]>0) cat(paste0("     ", name_spp_demand_i, " ", round(spp_demand[i],1), " m3/yr \n"))
          }
        }
      }
    }
    if(scenario_type == "input_rate"){
      extraction_rates <- management_scenario$extraction_rate_by_year
      if(!is.numeric(extraction_rates)) stop("Extraction rates should be a named numeric vector")
      if(is.null(names(extraction_rates))) stop("Extraction rates should be named")
      if(!all(as.character(years) %in% names(extraction_rates))) stop("Extraction rates have not been specified for all simulation years")
      extraction_rates <- extraction_rates[as.character(years)]
      if(verbose) {
        cli::cli_li(paste0("Input demand:\n"))
        if(is.vector(spp_demand)) {
          for(i in 1:length(spp_demand)) {
            name_spp_demand_i <- names(spp_demand)[i]
            if(nchar(name_spp_demand_i)> 30) name_spp_demand_i <- paste0(substr(name_spp_demand_i,1,27),"...")
            if(spp_demand[i]>0) cat(paste0("      ", name_spp_demand_i, " ", round(spp_demand[i],1), " m3/yr\n"))
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
  if(!("management_arguments" %in% names(y))) {
    if(progress) cli::cli_li(paste0("Adding column 'management_arguments'"))
    y$management_arguments = vector("list", n)
  }
  for(i in 1:n) {
    if(!is.na(y$management_unit[i])) {
      if(is.null(y$management_arguments[[i]])) y$management_arguments[[i]] = as.list(management_scenario$units[y$management_unit[i],])
    } else { # If management unit is missing remove any previously-existing management argument
      y$management_arguments[i] <- list(NULL)
    }
  }
  managed <- !is.na(y$management_unit)
  names(managed) <- y$id
  
  units = sort(unique(y$management_unit), na.last = TRUE)
  if(verbose) {
    cli::cli_li(paste0("Management units:"))
    for(i in 1:length(units)) {
      if(!is.na(units[i])) cat(paste0("     ", sum(y$management_unit==units[i], na.rm=TRUE), 
                                      " stand(s) in unit [", row.names(management_scenario$units)[units[i]],"]\n"))
      else cat(paste0("     ", sum(is.na(y$management_unit)), " stand(s) without prescriptions\n"))
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

  
  # Growth and extraction by species and year
  growth = matrix(0, nspp, length(years))
  rownames(growth) <- SpParams$Name
  extracted = matrix(0, nspp, length(years))
  rownames(extracted) <- SpParams$Name
  dead = matrix(0, nspp, length(years))
  rownames(dead) <- SpParams$Name
  
  # Demand growth, target and extracted volumes by target entity and year
  if(scenario_type != "bottom-up") {
    growth_target <- matrix(0, length(target_taxon_names), length(years))
    rownames(growth_target) <- target_taxon_names
    demand_target <- matrix(0, length(target_taxon_names), length(years))
    rownames(demand_target) <- target_taxon_names
    extracted_target <- matrix(0, length(target_taxon_names), length(years))
    rownames(extracted_target) <- target_taxon_names
    dead_target <- matrix(0, length(target_taxon_names), length(years))
    rownames(dead_target) <- target_taxon_names
  }
  
  initial_sum <- rep(0, length(years))
  growth_sum <- rep(0, length(years))
  dead_sum <- rep(0, length(years))
  final_sum <- rep(0, length(years))
  extracted_sum <- rep(0, length(years))
  initial_target_sum <- rep(0, length(years))
  growth_target_sum <- rep(0, length(years))
  dead_target_sum <- rep(0, length(years))
  final_target_sum <- rep(0, length(years))
  nominal_target_sum <- rep(0, length(years))
  volume_target_sum <- rep(0, length(years))
  extracted_target_sum <- rep(0, length(years))
  offset_target_sum <- rep(0, length(years))
  
  cumulative_extraction <- rep(0, length(years))
  cumulative_growth <- rep(0, length(years))
  cumulative_extraction_target <- rep(0, length(years))
  cumulative_growth_target <- rep(0, length(years))
  cumulative_nominal_target_sum <- rep(0, length(years))
  
  summary_list = vector("list", n)
  
  # Estimate initial standing volume
  initial_volume_spp <- .standingVolume(y, SpParams, volume_function, volume_arguments)
  if(progress) cli::cli_li(paste0("Initial volume: ", round(sum(initial_volume_spp)), " m3"))
  
  if(progress) {
    if(is.null(dispersal_control)) {
      cli::cli_li("Seed dispersal process not considered.")
    } else {
      cli::cli_li("Seed dispersal process included.")
    }
  }
  # B. Year loop
  if(progress) cli::cli_h2("Simulation")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cli::cli_h3(paste0(" [ Year ", year, " (", yi,"/", length(years),") ]"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    # Set demand in demand-based scenarios
    if(scenario_type=="input_demand") {
      nominal_target_sum[yi] <- sum(spp_demand)
      offset_target_sum[yi] <- sum(offset_demand, na.rm=TRUE)
      spp_demand_year <- spp_demand + offset_demand
      demand_target[, yi] = spp_demand_year
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
      nominal_target_sum[yi] <- sum(spp_demand_year)
      offset_target_sum[yi] <- sum(offset_demand, na.rm=TRUE)
      spp_demand_year <- spp_demand_year + offset_demand
      demand_target[,yi] = spp_demand_year
    }

    # B.1 Determine which plots will be managed according to current demand
    managed_step <- managed # by default, manage all plots that have
    if(scenario_type != "bottom-up") {
      spp_demand_thinning_year <- spp_demand_year
      if(progress) {
        volume_target_sum[yi] <- sum(spp_demand_year, na.rm=TRUE)
        cli::cli_li(paste0("  Demand (incl. offset): ", round(volume_target_sum[yi]), " m3"))
      }
      if(progress) cli::cli_li(paste0("Determining available volumes and final cuts"))
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
            argList <- list(x = ctd, SpParams = SpParams)
            if(!is.null(volume_arguments)) argList = c(argList, volume_arguments)
            vols_i <- do.call(what = volume_function, args = argList)*y$represented_area_ha[i]
            nsp_i <- medfate::plant_speciesName(f, SpParams)[1:nrow(f$treeData)]
            vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
            vol_sp_i <- vol_sp_i[names(vol_sp_i) %in% target_spp_names]
            if(length(vol_sp_i)>0) {
              vol_spp_target[i, names(vol_sp_i)] <- vol_sp_i
            }
            # DECREASE TARGET according to FINAL CUTS
            if(final_cuts[i]) {
              for(j in 1:length(target_taxon_names)) spp_demand_thinning_year[j] <- spp_demand_thinning_year[j] - vol_spp_target[i, target_split_names[[j]], drop=FALSE]
            }
          }
        }
      }
      if(progress) cli::cli_li(paste0("Demand (after final cuts): ", round(sum(spp_demand_thinning_year)), " m3"))
      
      
      # Determine fulfillment of demand via thinning
      if(progress) cli::cli_li(paste0("Determining thinning operations"))
      # Copy volume matrix from species to demand taxa
      vol_demand_target <- matrix(0, n, length(target_taxon_names)) 
      colnames(vol_demand_target) <- target_taxon_names
      rownames(vol_demand_target) <- y$id
      for(j in 1:length(target_taxon_names)) {
        vol_demand_target[,j] <- rowSums(vol_spp_target[, which(target_spp_names %in% target_split_names[[j]]), drop = FALSE])
      }
      
      # Set all plots not in final cuts to non-management
      managed_step[!final_cuts] <- FALSE 
      vol_demand_target_thinning <- vol_demand_target
      vol_demand_target_thinning[final_cuts, ] <- 0
      sel_positive_demand <- (spp_demand_thinning_year > 0)
      stand_positive_demand <- rowSums(vol_demand_target_thinning[, sel_positive_demand, drop = FALSE])
      stand_dominant_positive_demand <- stand_positive_demand/rowSums(vol_demand_target)
      sel_available_stands <- (stand_dominant_positive_demand > 0.5) & (stand_positive_demand < sum(pmax(0,spp_demand_thinning_year)))
      sel_available_stands[is.na(sel_available_stands)] <- FALSE
      while(any(sel_positive_demand) && any(sel_available_stands)) {
        i_max_vol_available <- which.max(stand_positive_demand[sel_available_stands])
        i_max_vol <- which(sel_available_stands)[i_max_vol_available]
        managed_step[i_max_vol] <- TRUE
        spp_demand_thinning_year <- spp_demand_thinning_year - vol_demand_target_thinning[i_max_vol, , drop = FALSE] 
        vol_demand_target_thinning[i_max_vol, ] <- 0
        sel_positive_demand <- (spp_demand_thinning_year > 0)
        stand_positive_demand <- rowSums(vol_demand_target_thinning[, sel_positive_demand, drop = FALSE])
        stand_dominant_positive_demand <- stand_positive_demand/rowSums(vol_demand_target)
        sel_available_stands <- (stand_dominant_positive_demand > 0.5)  & (stand_positive_demand < sum(pmax(0,spp_demand_thinning_year))) 
        sel_available_stands[is.na(sel_available_stands)] <- FALSE
      }

      if(verbose) {
        if(sum(managed_step)>0) {
          cli::cli_li(paste0(sum(managed_step), " stand(s) where management will be simulated (", sum(final_cuts) , " because of final cuts):"))
          for(i in 1:length(units)) {
            if(!is.na(units[i])) {
              n_man_unit <- sum(y$management_unit[managed_step]==units[i], na.rm=TRUE)
              if(n_man_unit>0) cat(paste0("      ", n_man_unit , 
                                          " stand(s) in unit [", row.names(management_scenario$units)[units[i]],"]\n"))
            }          
          }
          cli::cli_li(paste0("Volume by target species/group:"))
          for(j in 1:length(target_taxon_names)) {
            name_spp_demand_j <- target_taxon_names[j]
            vol_demand_j <- vol_demand_target[,j]
            if(nchar(name_spp_demand_j)> 30) name_spp_demand_j <- paste0(substr(name_spp_demand_j,1,27),"...")
            cat(paste0("      ", name_spp_demand_j, " - demand ", round(spp_demand_year[j]), " m3, expected ", round(sum(vol_demand_j[managed_step], na.rm=TRUE)), " m3 from ", sum(managed_step & (vol_demand_j>0)) ," stand(s).\n"))
          }
        } else {
          cli::cli_li("No stand(s) with management to be simulated")
        }
      }
    } else {
      if(progress) cli::cli_li(paste0(sum(managed_step), " stand(s) where management will be simulated"))
    }
    prev_management_args <- y$management_arguments
    y$management_arguments[managed & (!managed_step)] <- list(NULL) # Deactivates management on plots that were not selected
    
    if(!is.null(dispersal_control)) {
      if(progress) cli::cli_li(paste0("Seed bank dynamics and seed dispersal..."))
      seedbank_list <- dispersal(y, SpParams, local_control, 
                                 distance_step = dispersal_control[["distance_step"]],
                                 maximum_dispersal_distance = dispersal_control[["maximum_dispersal_distance"]],
                                 min_percent = dispersal_control[["min_percent"]],
                                 stochastic_resampling = dispersal_control[["stochastic_resampling"]],
                                 progress = FALSE)
      for(i in 1:n) { 
        y$forest[[i]]$seedBank <- seedbank_list[[i]]
      }
    }
    
    # B.2 Call fordyn_spatial()
    if(progress) cli::cli_li(paste0("Calling fordyn_spatial..."))
    fds <-.model_spatial(y, SpParams, meteo = meteo, model = "fordyn", local_control = local_control, dates = datesYear,
                        management_function = "defaultManagementFunction",
                        CO2ByYear = CO2ByYear, fire_regime = fire_regime,
                        keep_results = FALSE, summary_function=table_selection, 
                        summary_arguments=list(summary_function = summary_function, summary_arguments = summary_arguments),
                        parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, 
                        progress = progress, local_verbose = FALSE)

    # B.3 Update final state variables in y and retrieve fordyn tables
    # y_backup <- rlang::duplicate(y)
    y_backup <- y
    y <- update_landscape(y, fds) # This updates forest, soil, growthInput and management_arguments
    
    
    # B.3b Check for null forest objects (from errors in previous step)
    restored <- 0
    for(i in 1:n) {
      if(inherits(fds$result[[i]], "error")) {
        cli::cli_alert_warning(paste0("Error in plot ", i, ":", fds$result[[i]]))
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
        if(is.null(y$state[[i]])) y$state[[i]] <- growthInput(y$forest[[i]], y$soil[[i]], SpParams, local_control)
        
        if("management_arguments" %in% names(y_backup)) {
          man_backup  <- y_backup$management_arguments[[i]]
          if(!is.null(man_backup)) y$management_arguments[[i]] <- man_backup
          else y$management_arguments[[i]] <- list(NULL)
        }
        restored <- restored +1
      }
    }
    if(progress && (restored>0)) cli::cli_li(paste0("Restored ", restored, " stand(s) with errors during simulation to last year state"))
    
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
    # B.4 Store actual extraction and mortality
    for(i in 1:n){
      ctt <- fds$result[[i]]$CutTreeTable
      if(!is.null(ctt)) {
        argList <- list(x = ctt, SpParams = SpParams)
        if(!is.null(volume_arguments)) argList = c(argList, volume_arguments)
        vols_i <- do.call(what = volume_function, args = argList)*y$represented_area_ha[i]
        nsp_i <- medfate::species_characterParameter(ctt$Species, SpParams, "Name")
        vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
        extracted[names(vol_sp_i), yi] <- extracted[names(vol_sp_i), yi] + vol_sp_i
      }
      dtt <- fds$result[[i]]$DeadTreeTable
      if(!is.null(dtt)) {
        argList <- list(x = dtt, SpParams = SpParams)
        if(!is.null(volume_arguments)) argList = c(argList, volume_arguments)
        vols_i <- do.call(what = volume_function, args = argList)*y$represented_area_ha[i]
        nsp_i <- medfate::species_characterParameter(dtt$Species, SpParams, "Name")
        vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
        dead[names(vol_sp_i), yi] <- dead[names(vol_sp_i), yi] + vol_sp_i
      }
    }

    # B.5 Update extraction rates and actual satisfied demand
    final_volume_spp <- .standingVolume(y, SpParams, volume_function, volume_arguments)
    if(progress) cli::cli_li(paste0("Final volume: ", round(sum(final_volume_spp)), " m3"))
    growth[,yi] <- final_volume_spp - initial_volume_spp + extracted[,yi] + dead[,yi]
    extracted_sum[yi] <- sum(extracted[,yi], na.rm=TRUE)
    dead_sum[yi] <- sum(dead[,yi], na.rm=TRUE)
    initial_sum[yi] <- sum(initial_volume_spp, na.rm = TRUE)
    final_sum[yi] <- sum(final_volume_spp, na.rm = TRUE)
    growth_sum[yi] <- sum(growth[,yi], na.rm=TRUE)
    if(yi==1) {
      cumulative_extraction[yi] <- extracted_sum[yi]
      cumulative_growth[yi] <- growth_sum[yi]
    } else {
      cumulative_extraction[yi] <- cumulative_extraction[yi-1] + extracted_sum[yi]
      cumulative_growth[yi] <- cumulative_growth[yi-1] + growth_sum[yi]
    }
    if(scenario_type != "bottom-up") {
      # Copy extraction to matrix by demand
      for(j in 1:length(target_taxon_names)) {
        extracted_target[j,yi] <- sum(extracted[which(SpParams$Name %in% target_split_names[[j]]), yi])
        dead_target[j,yi] <- sum(dead[which(SpParams$Name %in% target_split_names[[j]]), yi])
        growth_target[j,yi] <- sum(growth[which(SpParams$Name %in% target_split_names[[j]]), yi])
      }
      dead_target_sum[yi] <- sum(dead[target_spp_names, yi], na.rm=TRUE)
      extracted_target_sum[yi] <- sum(extracted[target_spp_names, yi], na.rm=TRUE)
      initial_target_sum[yi] <- sum(initial_volume_spp[target_spp_names], na.rm = TRUE)
      final_target_sum[yi] <- sum(final_volume_spp[target_spp_names], na.rm = TRUE)
      growth_target_sum[yi] <- sum(growth[target_spp_names,yi], na.rm=TRUE)
      if(yi==1) {
        cumulative_extraction_target[yi] <- extracted_target_sum[yi]
        cumulative_growth_target[yi] <- growth_target_sum[yi]
        cumulative_nominal_target_sum[yi] <- nominal_target_sum[yi]
      } else {
        cumulative_extraction_target[yi] <- cumulative_extraction_target[yi-1] + extracted_target_sum[yi]
        cumulative_growth_target[yi] <- cumulative_growth_target[yi-1] + growth_target_sum[yi]
        cumulative_nominal_target_sum[yi] <- cumulative_nominal_target_sum[yi-1] + nominal_target_sum[yi]
      }
      # recalculate offset and previous growth for next year (or next simulation)
      offset_demand <- demand_target[, yi] - extracted_target[,yi]
      last_growth <- growth_target_sum[yi]
    }
    if(verbose) {
      cli::cli_li(paste0("Management statistics (all species):"))
      cat(paste0("      Initial (m3): ", round(initial_sum[yi]),
                 "      Growth (m3): ", round(growth_sum[yi]), 
                 "      Mortality (m3): ", round(dead_sum[yi]), 
                 "      Extraction (m3): ", round(extracted_sum[yi]), 
                 "      Final (m3): ", round(final_sum[yi]), 
                 "\n"))
      cat(paste0("      Extraction rate: ", round(100*extracted_sum[yi]/max(0,growth_sum[yi])),"%"))
      cat(paste0("      Average extraction rate: ", round(100*cumulative_extraction[yi]/max(0,cumulative_growth[yi])),"%\n"))
      if(scenario_type != "bottom-up") {
        cli::cli_li(paste0("Management statistics (target species with demand):"))
        cat(paste0("      Nominal demand (m3): ", round(sum(spp_demand, na.rm=TRUE)),
                   "      Offset (m3): ", round(offset_target_sum[yi]), 
                   "      Actual demand (m3): ", round(volume_target_sum[yi]),
                   "\n"))
        cat(paste0("      Initial (m3): ", round(initial_target_sum[yi]),
                   "      Growth (m3): ", round(growth_target_sum[yi]), 
                   "      Mortality (m3): ", round(dead_target_sum[yi]), 
                   "      Extraction (m3): ", round(extracted_target_sum[yi]), 
                   "      Final (m3): ", round(final_target_sum[yi]), 
                   "\n"))
        cat(paste0("      Extraction rate: ", round(100*extracted_sum[yi]/max(0,growth_target_sum[yi])),"%",
                   "      Average extraction rate: ", round(100*cumulative_extraction_target[yi]/max(0,cumulative_growth_target[yi])),"%",
                   "\n"))
        cat(paste0("      Demand satisfaction: ", round(100*extracted_target_sum[yi]/volume_target_sum[yi]), "%",
                   "      Nominal satisfaction: ", round(100*extracted_target_sum[yi]/nominal_target_sum[yi]), "%",
                   "      Average nominal satisfaction: ", round(100*cumulative_extraction_target[yi]/cumulative_nominal_target_sum[yi]), "%",
                   "\n"))
        cat(paste0("      Next year offset (m3): ", round(sum(offset_demand, na.rm=TRUE)), 
                   "\n"))
      }
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
  dead <- data.frame(Name = SpParams$Name, dead)
  names(dead) <- c("species", as.character(years))
  row.names(dead) <- NULL
  growth <- data.frame(Name = SpParams$Name, growth)
  names(growth) <- c("species", as.character(years))
  row.names(growth) <- NULL
  extracted_pv <- tidyr::pivot_longer(extracted, as.character(years), names_to="year", values_to = "extracted")
  dead_pv <- tidyr::pivot_longer(dead, as.character(years), names_to="year", values_to = "dead")
  growth_pv <- tidyr::pivot_longer(growth, as.character(years), names_to="year", values_to = "growth")
  volumes_spp <- growth_pv |>
    dplyr::full_join(dead_pv, by=c("species", "year")) |>
    dplyr::full_join(extracted_pv, by=c("species", "year")) |>
    dplyr::filter(growth!=0 | dead!=0 | extracted!=0) 
  if(management_scenario$scenario_type != "bottom-up") {
    growth_target <- data.frame(Name = target_taxon_names, growth_target)
    names(growth_target) <- c("species", as.character(years))
    row.names(growth_target) <- NULL
    extracted_target <- data.frame(Name = target_taxon_names, extracted_target)
    names(extracted_target) <- c("species", as.character(years))
    row.names(extracted_target) <- NULL
    dead_target <- data.frame(Name = target_taxon_names, dead_target)
    names(dead_target) <- c("species", as.character(years))
    row.names(dead_target) <- NULL
    demand_target <- data.frame(Name = target_taxon_names, demand_target)
    names(demand_target) <- c("species", as.character(years))
    row.names(demand_target) <- NULL
    growth_target_pv <- tidyr::pivot_longer(growth_target, as.character(years), names_to="year", values_to = "growth")
    demand_target_pv <- tidyr::pivot_longer(demand_target, as.character(years), names_to="year", values_to = "demand")
    dead_target_pv <- tidyr::pivot_longer(dead_target, as.character(years), names_to="year", values_to = "dead")
    extracted_target_pv <- tidyr::pivot_longer(extracted_target, as.character(years), names_to="year", values_to = "extracted")
    volumes_demand <- growth_target_pv |>
      dplyr::full_join(demand_target_pv, by=c("species", "year")) |> 
      dplyr::full_join(dead_target_pv, by=c("species", "year"))|> 
      dplyr::full_join(extracted_target_pv, by=c("species", "year"))
  }  
  
  volumes <- tibble::as_tibble(data.frame(Year = years, 
                                          initial = initial_sum, 
                                          growth = growth_sum, 
                                          mortality  = dead_sum, 
                                          extracted  = extracted_sum, 
                                          final = final_sum,
                                          cumulative_growth = cumulative_growth, 
                                          cumulative_extraction = cumulative_extraction, 
                                          initial_target = initial_target_sum,
                                          growth_target = growth_target_sum, 
                                          mortality_target = dead_target_sum, 
                                          extracted_target  = extracted_target_sum, 
                                          final_target = final_sum, 
                                          nominal_demand = nominal_target_sum,
                                          demand_offset = offset_target_sum,
                                          actual_demand = volume_target_sum, 
                                          cumulative_nominal_demand = cumulative_nominal_target_sum, 
                                          cumulative_extracted_demand = cumulative_extraction_target))
  l <- list(result_sf = sf::st_as_sf(tibble::as_tibble(sf_results)),
            result_volumes = volumes,
            result_volumes_spp = volumes_spp)
  if(scenario_type != "bottom-up") {
    l[["result_volumes_demand"]] <- volumes_demand
    l[["next_demand"]] <- list(offset = offset_demand, last_growth = last_growth)
  }
  l[["next_sf"]] <- y
  class(l)<-c("fordyn_scenario", "list")
  return(l)
}
