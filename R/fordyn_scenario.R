#' Scenario of forest dynamics
#' 
#' Evaluates forest dynamics over a landscape including climate and management scenarios
#'
#' @param y An object of class \code{\link{sf}} with landscape information.
#'    Management units should be defined in this object.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}). 
#' @param meteo Meteorology data (see \code{\link{fordyn_spatial}}).
#' @param localControl A list of local model control parameters (see \code{\link{defaultControl}}).
#' @param volumeFunction A function accepting a forest object as input and returning the wood volume (m3/ha) corresponding to each tree cohort.
#' If NULL, the default volume function is used (not recommended!).
#' @param managementScenario A list defining the management scenario (see \code{\link{create_management_scenario}})
#' @param dates A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param summaryFunction An appropriate function to calculate summaries from an object of class 'fordyn' (e.g., \code{\link{summary.fordyn}}).
#' @param summaryArgs List with additional arguments for the summary function.
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param numCores Integer with the number of cores to be used for parallel computation.
#' @param chunk.size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
#' 
#' @returns An list of class 'fordyn_scenario' with the following elements:
#'  \itemize{
#'    \item{An object of class 'sf' containing four elements:
#'      \itemize{
#'        \item{\code{geometry}: Spatial geometry.}
#'        \item{\code{id}: Stand id, taken from the input.}
#'        \item{\code{state}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{managementarguments}: A list of management arguments for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{treetable}: A list of data frames for each simulated stand, containing the living trees at each time step.}
#'        \item{\code{shrubtable}: A list of data frames for each simulated stand, containing the living shrub at each time step.}
#'        \item{\code{deadtreetable}: A list of data frames for each simulated stand, containing the dead trees at each time step.}
#'        \item{\code{deadshrubtable}: A list of data frames for each simulated stand, containing the dead shrub at each time step.}
#'        \item{\code{cuttreetable}: A list of data frames for each simulated stand, containing the cut trees at each time step.}
#'        \item{\code{cutshrubtable}: A list of data frames for each simulated stand, containing the cut shrub at each time step.}
#'        \item{\code{summary}: A list of model output summaries for each simulated stand (if \code{summaryFunction} was not \code{NULL}).}
#'      }
#'    }
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
#' meteo2002$Precipitation = meteo2002$Precipitation/2
#' row.names(meteo2002) = seq(as.Date("2002-01-01"), 
#'                          as.Date("2002-12-31"), by="day")
#' meteo_01_02 = rbind(meteo2001, meteo2002)
#'                          
#' # Load default medfate parameters
#' data("SpParamsMED")
#' 
#' # Creates default scenario with two management units
#' s = create_management_scenario(2)
#' 
#' # Modify thinning threshold of the arguments of management unit #1
#' s$managementUnits[[1]]$managementArgs$thinningThreshold = 15
#' 
#' # Define demand on a species basis
#' s$annualDemandBySpecies = c("Quercus ilex" = 1000, "Pinus nigra" = 2000)
#' 
#' # Subset 10 stands for computational simplicity
#' y = y[1:10, ]
#'
#' # Assign two management units to stands
#' y$managementunit[1:3] = 1 
#' y$managementunit[4:10] = 2
#' 
#' # Assume that each stand represents 1km2 = 100 ha
#' y$representedarea = 100
#' 
#' # Launch simulation scenario
#' res = fordyn_scenario(y, SpParamsMED, meteo_01_02, 
#'                       volumeFunction = NULL, managementScenario = s,
#'                       parallelize = TRUE)
#'}
#'
fordyn_scenario<-function(y, SpParams, meteo, 
                         managementScenario,
                         volumeFunction = NULL,
                         localControl = defaultControl(), dates = NULL,
                         CO2ByYear = numeric(0), summaryFunction=NULL, summaryArgs=NULL,
                         parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE){
  
  .check_model_inputs(y, meteo)
  
  demand_based <- (sum(managementScenario$annualDemandBySpecies) > 0)
  
  if(demand_based) {
    cat("Demand-based management scenario:\n")
    spp_demand = managementScenario$annualDemandBySpecies
    print(spp_demand)
    if(is.null(volumeFunction)) {
      cat("Using default volume function\n")
      volumeFunction = "defaultVolumeFunction"
    } 
  } else {
    cat("Bottom-up management scenario\n")
  }

  n = nrow(y)
  nspp = nrow(SpParams)
  
  if(any(is.na(y$representedarea))) stop("Column 'representedarea' cannot include missing values")
  
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
  table_selection<-function(object, summaryFunction, summaryArgs, ...) {
    l = object[c("TreeTable", "ShrubTable",
             "DeadTreeTable", "DeadTreeTable", 
             "CutTreeTable", "CutShrubTable")]
    if(!is.null(summaryFunction)) {
      argList = list(object=object)
      if(!is.null(summaryArgs)) argList = c(argList, summaryArgs)
      l[["summary"]] = do.call(summaryFunction, args=argList)
    }
    return(l)
  }

  
  # A.1 Initialize management arguments according to unit
  for(i in 1:n) {
    y$managementarguments[[i]] = managementScenario$managementUnits[[y$managementunit[i]]]$managementArgs
  }
  
  # A.2 Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  

  summary_list = vector("list", n)
  # B. Year loop
  if(progress) cat("SIMULATIONS: \n")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cat(paste0("[Year ", year, "]\n"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    # B.1 Determine which plots will be managed according to current demand
    if(demand_based) {
      vol_species = matrix(0, n, nspp) 
      colnames(vol_species) = SpParams$Name
      rownames(vol_species) = y$id
      final_cuts = rep(FALSE, n)
      for(i in 1:n) {
        man_args <- y$managementarguments[[i]] 
        f <- y$forest[[i]]
        if(!is.null(man_args)) {
          if((man_args$type=="regular") && (man_args$finalPreviousStage > 0)) final_cuts[i] = TRUE
          man <- do.call(what = managementScenario$managementFunction, 
                         args = list(x = f, args=man_args))
          ctd <- f$treeData
          ctd$N <- man$N_tree_cut
          vols_i <- sum(do.call(what = volumeFunction, args = list(x = ctd)))*y$representedarea[i]
          nsp_i <- medfate::plant_speciesName(f, SpParams)[1:nrow(f$treeData)]
          vol_species[i, nsp_i] = vols_i
        }
      }
      vol_spp_target = vol_species[,names(spp_demand)]
      print(data.frame(vol_spp_target = rowSums(vol_spp_target), final_cut = final_cuts))
    }
    
    
    # B.2 Call fordyn_spatial()
    fds <-fordyn_spatial(y, SpParams, meteo = meteo, localControl = localControl, dates = datesYear,
                        managementFunction = managementScenario$managementFunction,
                        CO2ByYear = CO2ByYear, keepResults = FALSE, summaryFunction=table_selection, 
                        summaryArgs=list(summaryFunction = summaryFunction, summaryArgs = summaryArgs),
                        parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, 
                        progress = progress)
    
    # B.3 Update final state variables in y and retrieve fordyn tables
    y = update_landscape(y, fds) # This updates forest, soil, growthInput and managementarguments
    # For those plots that were not managed but have prescriptions in a demand-based scenario, increase the variable years since thinning
    
    # Move summary into results
    fds$result = fds$summary 
    # Retrieve user-defined summaries (if existing)
    if(!is.null(summaryFunction)) {
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
    # B.4 Update actual satisfied demand
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
  sf$managementarguments = fds$managementarguments
  sf$treetable = tree_tables
  sf$shrubtable = shrub_tables
  sf$deadtreetable = dead_tree_tables
  sf$deadshrubtable = dead_shrub_tables
  sf$cuttreetable = cut_tree_tables
  sf$cutshrubtable = cut_shrub_tables
  sf$summary = summary_list
  
  l = list(sf = sf::st_as_sf(tibble::as_tibble(sf)))
  class(l)<-c("fordyn_scenario", "list")
  return(l)
}
