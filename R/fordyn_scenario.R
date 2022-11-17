
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
#' @param managementScenario A list defining the management scenario (see \code{\link{create_management_scenario}})
#' @param dates A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param numCores Integer with the number of cores to be used for parallel computation.
#' @param chunk.size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
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
#' # Subset 10 stands for computational simplicity
#' y = y[1:10, ]
#'
#' # Assign two management units to stands
#' y$managementunit[1:3] = 1 
#' y$managementunit[4:10] = 2
#' 
#' res = fordyn_scenario(y, SpParamsMED, meteo_01_02, 
#'                       volumeFunction = NULL, managementScenario = s,
#'                       parallelize = TRUE)
#'}
fordyn_scenario<-function(y, SpParams, meteo, 
                         volumeFunction, managementScenario,
                         localControl = defaultControl(), dates = NULL,
                         CO2ByYear = numeric(0),
                         parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE){
  
  management_function = managementScenario$managementFunction
  
  
  
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
  table_selection<-function(object, ...) {object[c("TreeTable", "CutTreeTable", "ShrubTable", "CutShrubTable")]}
  
  # A.1 Initialize management arguments according to unit
  for(i in 1:nrow(y)) {
    y$managementarguments[[i]] = managementScenario$managementUnits[[y$managementunit[i]]]$managementArgs
  }
  
  # A.2 Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  # B. Year loop
  if(progress) cat("SIMULATIONS: \n")
  for(yi in 1:length(years)) {
    year = years[yi]
    if(progress) cat(paste0("[Year ", year, "]\n"))
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    # B.1 Determine which plots will be managed according to current demand

    # B.2 Call fordynspatial()
    fds <-fordyn_spatial(y, SpParams, meteo = meteo, localControl = localControl, dates = datesYear,
                        managementFunction = management_function,
                        CO2ByYear = CO2ByYear, keepResults = FALSE, summaryFunction=table_selection, summaryArgs=NULL,
                        parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, 
                        progress = progress)
    
    # B.3 Update final state variables in y and retrieve fordyn tables
    y = update_landscape(y, fds) # This updates forest, soil, growthInput and managementarguments
    fds$result = fds$summary # Move summary into results
    
    if(yi==1) {
      tree_table = fordyn_tables(fds, "TreeTable")
      shrub_table = fordyn_tables(fds, "ShrubTable")
      cut_tree_table = fordyn_tables(fds, "CutTreeTable")
      cut_shrub_table = fordyn_tables(fds, "CutShrubTable")
    } else {
      tt_i = fordyn_tables(fds, "TreeTable")
      tree_table = dplyr::bind_rows(tree_table, tt_i[tt_i$Step==1,])
      st_i = fordyn_tables(fds, "ShrubTable")
      shrub_table = dplyr::bind_rows(shrub_table, st_i[st_i$Step==1,])
      cut_tree_table = dplyr::bind_rows(cut_tree_table, fordyn_tables(fds, "CutTreeTable"))
      cut_shrub_table = dplyr::bind_rows(cut_shrub_table, fordyn_tables(fds, "CutShrubTable"))
    }
    
    # for(i in 1:nrow(y)) print(fds$result[[i]]$CutTreeTable)
    # B.4 Update actual satisfied demand
  }
  if(progress) cat("ARRANGING OUTPUT: \n")
  tree_table = tree_table[,names(tree_table)!="Step"]
  shrub_table = shrub_table[,names(shrub_table)!="Step"]
  cut_tree_table = cut_tree_table[,names(cut_tree_table)!="Step"]
  cut_shrub_table = cut_shrub_table[,names(cut_shrub_table)!="Step"]
  l = list(final_landscape = y,
           TreeTable = tree_table,
           ShrubTable = shrub_table,
           CutTreeTable = cut_tree_table,
           CutShrubTable = cut_shrub_table)
  class(l)<-c("fordyn_scenario", "list")
  return(l)
}
