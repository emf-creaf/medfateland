#' Scenario of forest dynamics
#' 
#' Evaluates forest dynamics over a landscape including climate and management scenarios
#'
#' @param y An object of class \code{\link{sf}} with landscape information.
#'    Management units should be defined in this object.
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
#' data("examplepointslandscape")
#'   
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplepointslandscape)
#'
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' #Prepare a two-year meteorological data with half precipitation during 
#' #the second year
#' meteo2001 = examplemeteo
#' meteo2002 = examplemeteo
#' meteo2003 = examplemeteo
#' 
#' # Induce drought in year 2002
#' meteo2002$Precipitation = meteo2002$Precipitation/2
#' 
#' # Bind data and redefine row names (dates)
#' meteo_01_03 = rbind(meteo2001, meteo2002, meteo2003)
#' row.names(meteo_01_03) = seq(as.Date("2001-01-01"), 
#'                              as.Date("2003-12-31"), by="day")
#'                          
#' # Load default medfate parameters
#' data("SpParamsMED")
#' 
#' # Creates scenario with two management units and annual demand for two species 
#' s = create_management_scenario(2,  
#'                                c("Quercus ilex" = 1000, "Pinus nigra" = 2000))
#' 
#' # Modify thinning threshold of the arguments of management unit #1
#' s$units[[1]]$thinningThreshold = 15
#' 
#' # Subset 10 stands for computational simplicity
#' y = y[1:10, ]
#'
#' # Assign two management units to stands
#' y$management_unit[1:3] = 1 
#' y$management_unit[4:10] = 2
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' y$represented_area = 100
#' 
#' # Launch simulation scenario
#' res = fordyn_scenario(y, SpParamsMED, meteo_01_03, 
#'                       volume_function = NULL, management_scenario = s,
#'                       parallelize = TRUE)
#'}
#'
fordyn_scenario<-function(y, SpParams, meteo, 
                         management_scenario,
                         volume_function = NULL,
                         local_control = defaultControl(), dates = NULL,
                         CO2ByYear = numeric(0), summary_function=NULL, summary_arguments=NULL,
                         parallelize = FALSE, num_cores = detectCores()-1, chunk_size = NULL, progress = TRUE){
  
  .check_model_inputs(y, meteo)

  
  cat("SCENARIO PARAMETERS:\n")
  cat(paste0("   Scenario type: ", management_scenario$scenario_type,"\n"))
  
  spp_demand = rep(0, nrow(SpParams))
  names(spp_demand) = SpParams$Name
  if(management_scenario$scenario_type != "bottom-up"){
    spp_demand[names(management_scenario$annual_demand_by_species)] = management_scenario$annual_demand_by_species
    if(management_scenario$scenario_type == "input_demand"){
      cat(paste0("   Demand:\n"))
      if(is.vector(spp_demand)) {
        for(i in 1:length(spp_demand)) {
          if(spp_demand[i]>0) cat(paste0("     ", names(spp_demand)[i], " ", spp_demand[i], " m3/yr \n"))
        }
      }
    }
    if(management_scenario$scenario_type == "input_rate"){
      extraction_rate = management_scenario$extraction_rate_by_year
      cat(paste0("   Initial demand:\n"))
      if(is.vector(spp_demand)) {
        for(i in 1:length(spp_demand)) {
          cat(paste0("      ", names(spp_demand)[i], " ", spp_demand[i], " m3/yr\n"))
        }
      }
      cat(paste0("   Extraction rates:\n"))
      for(i in 1:length(extraction_rate)) {
        cat(paste0("      ", names(extraction_rate)[i], " ", extraction_rate[i], " %\n"))
      }
    }
  } 
  if(is.null(volume_function)) {
    cat("   Default volume function\n")
    volume_function = "default_volume_function"
  } else {
    cat("   User-defined volume function\n")
  }
  
  cat("\n")

  n = nrow(y)
  nspp = nrow(SpParams)
  
  if(any(is.na(y$represented_area))) stop("Column 'represented_area' cannot include missing values")
  
  if(inherits(meteo,"data.frame")) {
    datesMeteo = as.Date(row.names(meteo))
  } else if(inherits(meteo, "MeteorologyInterpolationData")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialGridMeteorology")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialPixelsMeteorology")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "stars")) {
    datesMeteo = as.Date(stars::st_get_dimension_values(meteo, "date"))
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  
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

  
  # A.1 Initialize management arguments according to unit
  for(i in 1:n) {
    y$management_arguments[[i]] = management_scenario$units[[y$management_unit[i]]]
  }
  
  # A.2 Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  

  offset_demand = rep(0, nspp)
  names(offset_demand) = SpParams$Name
  extracted = matrix(0, nspp, length(years))
  rownames(extracted) = SpParams$Name
  target = extracted
  
  summary_list = vector("list", n)
  # B. Year loop
  if(progress) cat("SIMULATIONS: \n")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cat(paste0("[Year ", year, "]\n"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    spp_demand_year = spp_demand + offset_demand
    target[,yi] = spp_demand_year
    
    # B.1 Determine which plots will be managed according to current demand
    if(management_scenario$scenario_type != "bottom-up") {
      vol_species = matrix(0, n, nspp) 
      colnames(vol_species) = SpParams$Name
      rownames(vol_species) = y$id
      final_cuts = rep(FALSE, n)
      for(i in 1:n) {
        man_args <- y$management_arguments[[i]] 
        f <- y$forest[[i]]
        if(!is.null(man_args)) {
          if((man_args$type=="regular") && (man_args$finalPreviousStage > 0)) final_cuts[i] = TRUE
          man <- do.call(what = "defaultManagementFunction", 
                         args = list(x = f, args=man_args))
          ctd <- f$treeData
          ctd$N <- man$N_tree_cut
          vols_i <- sum(do.call(what = volume_function, args = list(x = ctd)))*y$represented_area[i]
          nsp_i <- medfate::plant_speciesName(f, SpParams)[1:nrow(f$treeData)]
          vol_species[i, nsp_i] = vols_i
        }
      }
      vol_spp_target = vol_species[,names(spp_demand)]
      # print(data.frame(vol_spp_target = rowSums(vol_spp_target), final_cut = final_cuts))
    }
    
    
    # B.2 Call fordyn_spatial()
    fds <-fordyn_spatial(y, SpParams, meteo = meteo, local_control = local_control, dates = datesYear,
                        managementFunction = "defaultManagementFunction",
                        CO2ByYear = CO2ByYear, keep_results = FALSE, summary_function=table_selection, 
                        summary_arguments=list(summary_function = summary_function, summary_arguments = summary_arguments),
                        parallelize = parallelize, num_cores = num_cores, chunk_size = chunk_size, 
                        progress = progress)
    
    # B.3 Update final state variables in y and retrieve fordyn tables
    y = update_landscape(y, fds) # This updates forest, soil, growthInput and management_arguments
    # For those plots that were not managed but have prescriptions in a demand-based scenario, increase the variable years since thinning
    
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
      tree_table = fordyn_tables(fds, "TreeTable")
      shrub_table = fordyn_tables(fds, "ShrubTable")
      dead_tree_table = fordyn_tables(fds, "DeadTreeTable")
      dead_shrub_table = fordyn_tables(fds, "DeadShrubTable")
      cut_tree_table = fordyn_tables(fds, "CutTreeTable")
      cut_shrub_table = fordyn_tables(fds, "CutShrubTable")
    } else {
      tt_i = fordyn_tables(fds, "TreeTable")
      tree_table = dplyr::bind_rows(tree_table, tt_i[tt_i$Step==1,])
      st_i = fordyn_tables(fds, "ShrubTable")
      shrub_table = dplyr::bind_rows(shrub_table, st_i[st_i$Step==1,])
      dead_tree_table = dplyr::bind_rows(dead_tree_table, fordyn_tables(fds, "DeadTreeTable"))
      dead_shrub_table = dplyr::bind_rows(dead_shrub_table, fordyn_tables(fds, "DeadShrubTable"))
      cut_tree_table = dplyr::bind_rows(cut_tree_table, fordyn_tables(fds, "CutTreeTable"))
      cut_shrub_table = dplyr::bind_rows(cut_shrub_table, fordyn_tables(fds, "CutShrubTable"))
    }
    # B.4 Store actual extraction
    for(i in 1:n){
      f = y$forest[[i]]
      vols_i <- sum(do.call(what = volume_function, 
                            args = list(x = fds$result[[i]]$CutTreeTable)))*y$represented_area[i]
      nsp_i <- medfate::plant_speciesName(f, SpParams)[1:nrow(f$treeData)]
      extracted[nsp_i, yi] <- extracted[nsp_i, yi] + vols_i
    }
    # B.5 Update actual satisfied demand
    if(management_scenario$scenario_type != "bottom-up") {
      # recalculate offset for next year
      offset_demand = target[, yi] - extracted[,yi]
    }
  }
  if(progress) cat("ARRANGING OUTPUT: \n")
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
  sf$id = fds$id
  sf$state = fds$state
  sf$forest = fds$forest
  sf$management_arguments = fds$management_arguments
  sf$tree_table = tree_tables
  sf$shrub_table = shrub_tables
  sf$dead_tree_table = dead_tree_tables
  sf$dead_shrub_table = dead_shrub_tables
  sf$cut_tree_table = cut_tree_tables
  sf$cut_shrub_table = cut_shrub_tables
  sf$summary = summary_list
  
  # Volumes extracted
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
