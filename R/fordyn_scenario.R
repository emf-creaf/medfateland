#' Scenario of forest dynamics
#' 
#' Evaluates forest dynamics over a landscape including climate and management scenarios
#'
#' @param y An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.
#'    Management units should be defined in this object.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}). 
#' @param meteo Meteorology data (see \code{\link{fordynspatial}}).
#' @param localControl A list of local model control parameters (see \code{\link{defaultControl}}).
#' @param volumeFunction A function accepting a forest object as input and returning the wood volume corresponding to each tree cohort.
#' @param managementScenario A list defining the management scenario (see \code{\link{createManagementScenario}})
#' @param dates A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param parallelize Boolean flag to try parallelization (will use all clusters minus one).
#' @param numCores Integer with the number of cores to be used for parallel computation.
#' @param chunk.size Integer indicating the size of chunks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).
#' @param progress Boolean flag to display progress information for simulations.
fordyn_scenario<-function(y, SpParams, meteo, 
                         volumeFunction, managementScenario,
                         localControl = defaultControl(), dates = NULL,
                         CO2ByYear = numeric(0),
                         parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, progress = TRUE){
  
  # A. Determine number of years to process
  years = unique(sort(as.numeric(format(dates, "%Y"))))
  # B. Year loop
  for(yi in 1:length(years)) {
    year = years[yi]
    datesYear = dates[as.numeric(format(dates, "%Y")) == year]

    # B.1 Determine which plots will be managed according to current demand
    
    # B.2 Call fordynspatial()
    fds <-fordyn_spatial(y, SpParams, meteo = meteoYear, localControl = localControl, dates = datesYear,
                        managementFunction = managementFunction,
                        CO2ByYear = CO2ByYear, keepResults = TRUE, summaryFunction=NULL, summaryArgs=NULL,
                        parallelize = parallelize, numCores = numCores, chunk.size = chunk.size, progress = progress)
    # B.3 Update final state variables in y and retrieve fordyn tables
    y = update_state(y, fds)
    
    # B.4 Update actual satisfied demand
  }
}