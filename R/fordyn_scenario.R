.standingVolume<-function(y, target_spp_names, SpParams, volume_function){
  n = nrow(y)
  volume_target_spp = rep(0, length(target_spp_names))
  names(volume_target_spp) = target_spp_names
  for(i in 1:n) {
    f = y$forest[[i]]
    vols_i <- do.call(what = volume_function, 
                      args = list(x = f$treeData))*y$represented_area[i]
    nsp_i <- medfate::species_characterParameter(f$treeData$Species, SpParams, "Name")
    vol_sp_i <- tapply(vols_i, nsp_i, FUN = sum, na.rm=TRUE)
    vol_target_i <- vol_sp_i[names(vol_sp_i) %in% target_spp_names]
    if(length(vol_target_i)>0) volume_target_spp[names(vol_target_i)] = volume_target_spp[names(vol_target_i)] + vol_target_i
  }
  return(sum(volume_target_spp))
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
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}). 
#' @param meteo Meteorology data (see \code{\link{fordyn_spatial}}).
#' @param local_control A list of local model control parameters (see \code{\link{defaultControl}}).
#' @param volume_function A function accepting a forest object as input and returning the wood volume (m3/ha) corresponding to each tree cohort.
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
#'    \item{\code{sf}: An object of class 'sf' containing four elements:
#'      \itemize{
#'        \item{\code{geometry}: Spatial geometry.}
#'        \item{\code{id}: Stand id, taken from the input.}
#'        \item{\code{state}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{management_arguments}: A list of management arguments for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{tree_table}: A list of data frames for each simulated stand, containing the living trees at each time step.}
#'        \item{\code{shrub_table}: A list of data frames for each simulated stand, containing the living shrub at each time step.}
#'        \item{\code{dead_tree_table}: A list of data frames for each simulated stand, containing the dead trees at each time step.}
#'        \item{\code{dead_shrub_table}: A list of data frames for each simulated stand, containing the dead shrub at each time step.}
#'        \item{\code{cut_tree_table}: A list of data frames for each simulated stand, containing the cut trees at each time step.}
#'        \item{\code{cut_shrub_table}: A list of data frames for each simulated stand, containing the cut shrub at each time step.}
#'        \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summary_function} was not \code{NULL}).}
#'      }
#'    }
#'    \item{\code{volumes}: A data frame with extracted volumes (m3) by species and year. In demand-based scenarios target volumes are also included.}
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
#' #Prepare a three-year meteorological data 
#' meteo_01_03 <- rbind(examplemeteo, examplemeteo, examplemeteo)
#' row.names(meteo_01_03) <- seq(as.Date("2001-01-01"), 
#'                               as.Date("2003-12-31"), by="day")
#'                          
#' # Load default medfate parameters
#' data("SpParamsMED")
#' 
#' # Creates scenario with one management unit and annual demand for P. nigra 
#' scen <- create_management_scenario(1, c("Pinus nigra" = 2300))
#' 
#' # Assign management unit to all stands
#' example_ifn$management_unit <- 1 
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' example_ifn$represented_area <- 100
#' 
#' # Launch simulation scenario
#' res <- fordyn_scenario(example_ifn, SpParamsMED, meteo = meteo_01_03, 
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
  
  y <- sf
  
  .check_model_inputs(y, meteo)

  # A.1 Check inputs
  n = nrow(y)
  nspp = nrow(SpParams)

  if(!("represented_area" %in% names(y))) stop("Column 'represented_area' must be defined in 'y'")
  if(!("management_unit" %in% names(y))) stop("Column 'management_unit' must be defined in 'y'")
  
  if(any(is.na(y$represented_area))) stop("Column 'represented_area' cannot include missing values")
  
  scenario_type = match.arg(management_scenario$scenario_type, c("bottom-up", "input_rate", "input_demand"))
  
  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) {
      datesMeteo = as.Date(row.names(meteo))
    } else if(inherits(meteo, "stars")) {
      datesMeteo = as.Date(stars::st_get_dimension_values(meteo, "date"))
    }
  } 
  else {
    if(!("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'y' if not supplied separately")
    datesMeteo = as.Date(row.names(y$meteo[[1]]))
    # check that all items have same dates
    for(i in 1:nrow(y)) {
      if(!all(as.Date(row.names(y$meteo[[i]]))==datesMeteo)) stop("All spatial elements need to have the same weather dates.")
    }
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  # Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  
  # A.2 Scenario parameters
  cat("SCENARIO PARAMETERS:\n")
  cat(paste0("  Number of stands: ", n,"\n"))
  cat(paste0("  Number of years: ", length(years),"\n"))
  cat(paste0("  Management scenario type: ", scenario_type,"\n"))
  target_spp_names = names(management_scenario$annual_demand_by_species)
  spp_demand = rep(0, nrow(SpParams))
  names(spp_demand) = SpParams$Name
  if(scenario_type != "bottom-up"){
    spp_demand[target_spp_names] = management_scenario$annual_demand_by_species
    if(scenario_type == "input_demand"){
      cat(paste0("  Fixed demand:\n"))
      if(is.vector(spp_demand)) {
        for(i in 1:length(spp_demand)) {
          if(spp_demand[i]>0) cat(paste0("     ", names(spp_demand)[i], " ", spp_demand[i], " m3/yr \n"))
        }
      }
    }
    if(scenario_type == "input_rate"){
      extraction_rates = management_scenario$extraction_rate_by_year
      cat(paste0("  Initial demand:\n"))
      if(is.vector(spp_demand)) {
        for(i in 1:length(spp_demand)) {
          if(spp_demand[i]>0) cat(paste0("      ", names(spp_demand)[i], " ", spp_demand[i], " m3/yr\n"))
        }
      }
      cat(paste0("  Extraction rates:\n"))
      for(i in 1:length(extraction_rates)) {
        cat(paste0("      yr. ", names(extraction_rates)[i], " - ", extraction_rates[i], "%\n"))
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
  cat(paste0("  Management units:\n"))
  for(i in 1:length(units)) {
    if(!is.na(units[i])) cat(paste0("     ", sum(y$management_unit==units[i], na.rm=TRUE), 
                                    " stands in unit [", row.names(management_scenario$units)[i],"]\n"))
    else cat(paste0("     ", sum(is.na(y$management_unit)), " stands without prescriptions\n"))
  }
  if(is.null(volume_function)) {
    cat("  Default volume function\n")
    volume_function = "default_volume_function"
  } else {
    cat("  User-defined volume function\n")
  }
  cat("\n")
  
  
  
  # Define summary function (to save memory)
  table_selection<-function(object, summary_function, summary_arguments, ...) {
    l = object[c("TreeTable", "ShrubTable",
             "DeadTreeTable", "DeadTreeTable", 
             "CutTreeTable", "CutShrubTable")]
    if(!is.null(summary_function)) {
      argList = list(object=object)
      if(!is.null(summary_arguments)) argList = c(argList, summary_arguments)
      l[["summary"]] = do.call(summary_function, args=argList)
    }
    return(l)
  }

  
  extracted = matrix(0, nspp, length(years))
  rownames(extracted) = SpParams$Name
  target = extracted
  cumulative_extraction = 0
  cumulative_growth = 0
  summary_list = vector("list", n)
  
  # Estimate initial standing volume
  initial_volume_target_spp = .standingVolume(y, target_spp_names, SpParams, volume_function)
  
  # Initial demand
  if(scenario_type != "bottom-up") spp_demand_year = spp_demand
  
  # B. Year loop
  if(progress) cat("SIMULATIONS: \n")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cat(paste0(" [ Year ", year, " (", yi,"/", length(years),") ]\n"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    target[,yi] = spp_demand_year
    
    # B.1 Determine which plots will be managed according to current demand
    managed_step = managed # by default, manage all plots that have
    if(scenario_type != "bottom-up") {
      if(progress) {
        if(scenario_type=="input_rate") {
          if(yi>1) cat(paste0("  Target extraction rate: ", extraction_rates[as.character(year)], "%\n"))
        }
        cat(paste0("  Species demand:\n"))
        for(i in 1:length(spp_demand_year)) {
          if(spp_demand_year[i]>0) cat(paste0("      ", names(spp_demand_year)[i], " ", round(spp_demand_year[i]), " m3/yr\n"))
        }
      }
      vol_species = matrix(0, n, nspp) 
      colnames(vol_species) = SpParams$Name
      rownames(vol_species) = y$id
      final_cuts = rep(FALSE, n)
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
            vol_species[i, names(vol_sp_i)] = vol_sp_i
            # DECREASE TARGET according to FINAL CUTS
            if(final_cuts[i]) spp_demand_year = spp_demand_year - vol_species[i,, drop=FALSE]
          }
        }
      }
      vol_spp_target = vol_species
      # print(data.frame(vol_spp_target = rowSums(vol_spp_target), final_cut = final_cuts))
      # Set all plots not in final cuts to non-management
      no_final = which(!final_cuts)
      managed_step[no_final] = FALSE 
      to_cut = integer(0)
      # For each species set to true
      for(j in 1:length(spp_demand_year)) {
        vol_spp_j = vol_spp_target[,j]
        if(spp_demand_year[j]>0) {
          # cat(paste0("Species ", names(spp_demand_year)[j], " Target ", spp_demand_year[j],"\n"))
          o = order(vol_spp_j, decreasing = TRUE)
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
      cat(paste0("  ", sum(managed_step), " stands to be managed (", sum(final_cuts) , " because of final cuts)\n"))
    } else {
      cat(paste0("  ", sum(managed_step), " stands allowed to be managed\n"))
    }
    prev_management_args = y$management_arguments
    y$management_arguments[managed & (!managed_step)] = list(NULL) # Deactivates management on plots that were not selected
    
    # B.2 Call fordyn_spatial()
    if(progress) cat(paste0("  Calling fordyn_spatial...\n"))
    fds <-fordyn_spatial(y, SpParams, meteo = meteo, local_control = local_control, dates = datesYear,
                        management_function = "defaultManagementFunction",
                        CO2ByYear = CO2ByYear, keep_results = FALSE, summary_function=table_selection, 
                        summary_arguments=list(summary_function = summary_function, summary_arguments = summary_arguments),
                        parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, 
                        progress = FALSE)
    
    # B.3 Update final state variables in y and retrieve fordyn tables
    y = update_landscape(y, fds) # This updates forest, soil, growthInput and management_arguments
    # For those plots that were not managed but have prescriptions in a demand-based scenario, 
    # copy back management arguments and increase the variable years since thinning
    y$management_arguments[managed & (!managed_step)] <- prev_management_args[managed & (!managed_step)]
    for(i in 1:n){
      if(managed[i] && (!managed_step[i])){
        if(!is.na(y$management_arguments[[i]]$yearsSinceThinning)) y$management_arguments[[i]]$yearsSinceThinning = y$management_arguments[[i]]$yearsSinceThinning + 1
      }
    }
    # Move summary into results
    fds$result = fds$summary 
    # Retrieve user-defined summaries (if existing)
    if(!is.null(summary_function)) {
      for(i in 1:n){
        if(yi==1) summary_list[[i]] = fds$result[[i]]$summary
        else summary_list[[i]] = rbind(summary_list[[i]], fds$result[[i]]$summary)
      }
    }
    # Retrieve tree tables
    if(yi==1) {
      tree_table = .fordyn_tables(fds, "TreeTable")
      shrub_table = .fordyn_tables(fds, "ShrubTable")
      dead_tree_table = .fordyn_tables(fds, "DeadTreeTable")
      dead_shrub_table = .fordyn_tables(fds, "DeadShrubTable")
      cut_tree_table = .fordyn_tables(fds, "CutTreeTable")
      cut_shrub_table = .fordyn_tables(fds, "CutShrubTable")
    } else {
      tt_i = .fordyn_tables(fds, "TreeTable")
      tree_table = dplyr::bind_rows(tree_table, tt_i[tt_i$Step==1,])
      st_i = .fordyn_tables(fds, "ShrubTable")
      shrub_table = dplyr::bind_rows(shrub_table, st_i[st_i$Step==1,])
      dead_tree_table = dplyr::bind_rows(dead_tree_table, .fordyn_tables(fds, "DeadTreeTable"))
      dead_shrub_table = dplyr::bind_rows(dead_shrub_table, .fordyn_tables(fds, "DeadShrubTable"))
      cut_tree_table = dplyr::bind_rows(cut_tree_table, .fordyn_tables(fds, "CutTreeTable"))
      cut_shrub_table = dplyr::bind_rows(cut_shrub_table, .fordyn_tables(fds, "CutShrubTable"))
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
    final_volume_target_spp = .standingVolume(y, target_spp_names, SpParams, volume_function)
    extracted_i = sum(extracted[,yi])
    if(scenario_type != "bottom-up") {
      target_i = sum(target[,yi])
      if(progress) {
        cat(paste0("  Target (m3): ", round(target_i), 
                   "  Extraction (m3): ", round(extracted_i), " (", round(100*extracted_i/target_i),"% of target)"))
      }
    } else {
      if(progress) {
        cat(paste0("  Extraction (m3): ", round(extracted_i)))
      }
    }
    growth_i = final_volume_target_spp - initial_volume_target_spp + extracted_i
    cumulative_extraction = cumulative_extraction + extracted_i
    cumulative_growth = cumulative_growth + growth_i
    if(progress) {
      cat(paste0("  Growth (m3): ", round(growth_i), "\n",
                 "  Extraction rate: ", round(100*extracted_i/growth_i),"%"))
      if(yi>1) cat(paste0("  Average extraction rate: ", round(100*cumulative_extraction/cumulative_growth),"%\n"))
      else cat("\n")
    }
    if(progress) cat("\n")
    # recalculate demand for next year
    if(yi < length(years)) {
      if(scenario_type=="input_demand") {
        offset_demand = target[, yi] - extracted[,yi]
        spp_demand_year = spp_demand + offset_demand
      } else if(scenario_type=="input_rate") {
        offset_demand = target[, yi] - extracted[,yi]
        extraction_rate_next = extraction_rates[as.character(years[yi+1])]
        total_extraction_next = growth_i*(extraction_rate_next/100)
        spp_demand_year = total_extraction_next*(spp_demand/sum(spp_demand)) + offset_demand
      }
    }
    # Initial standing volume for next year
    initial_volume_target_spp = final_volume_target_spp
  }
  if(progress) cat("ARRANGING OUTPUT: \n")
  if(progress) cat(" Tree/shrub tables\n")
  tree_tables = vector("list", n)
  shrub_tables = vector("list", n)
  dead_tree_tables = vector("list", n)
  dead_shrub_tables = vector("list", n)
  cut_tree_tables = vector("list", n)
  cut_shrub_tables = vector("list", n)
  for(i in 1:n) {
    tree_tables[[i]] = tree_table[tree_table$id==y$id[i], ]
    shrub_tables[[i]] = shrub_table[shrub_table$id==y$id[i], ]
    dead_tree_tables[[i]] = dead_tree_table[dead_tree_table$id==y$id[i], ]
    dead_shrub_tables[[i]] = dead_shrub_table[dead_shrub_table$id==y$id[i], ]
    cut_tree_tables[[i]] = cut_tree_table[cut_tree_table$id==y$id[i], ]
    cut_shrub_tables[[i]] = cut_shrub_table[cut_shrub_table$id==y$id[i], ]
  }
  sf = sf::st_sf(geometry=sf::st_geometry(y))
  sf$id = y$id
  sf$state = y$state
  sf$forest = y$forest
  sf$management_arguments = y$management_arguments
  sf$tree_table = tree_tables
  sf$shrub_table = shrub_tables
  sf$dead_tree_table = dead_tree_tables
  sf$dead_shrub_table = dead_shrub_tables
  sf$cut_tree_table = cut_tree_tables
  sf$cut_shrub_table = cut_shrub_tables
  sf$summary = summary_list
  
  # Volumes extracted
  if(progress) cat(" Extracted volumes\n")
  extractSums = rowSums(extracted, na.rm=TRUE)
  extracted = data.frame(Name = SpParams$Name[extractSums>0], extracted[extractSums>0,])
  names(extracted) <- c("species", as.character(years))
  row.names(extracted) <- NULL
  target = data.frame(Name = SpParams$Name[extractSums>0], target[extractSums>0,])
  names(target) <- c("species", as.character(years))
  row.names(target) <- NULL
  extracted_pv <- tidyr::pivot_longer(extracted, as.character(2001:2003), names_to="year", values_to = "extracted")
  target_pv <- tidyr::pivot_longer(target, as.character(2001:2003), names_to="year", values_to = "target")
  if(management_scenario$scenario_type != "bottom-up") {
    volumes <- dplyr::full_join(target_pv, extracted_pv, by=c("species", "year"))
  } else {
    volumes = extracted_pv
  }
  l = list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
           volumes = volumes)
  class(l)<-c("fordyn_scenario", "list")
  return(l)
}
