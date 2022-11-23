#' Create management scenario
#' 
#' Defines a management scenario with default values, to be modified by the user
#'
#' @param numberOfUnits Number of management units. Each management unit represents a group of forest stands following the same silvicultural model.
#' @param annualDemandBySpecies A vector or matrix of annual wood demand (m3) by medfate species names. If empty, the scenario is 'bottom-up' (not based on demand).
#'                   If a vector is supplied, the same wood demand is applied for all simulated years. If a matrix is supplied, each row should correspond
#'                   to a different year.
#' @param extractionRateByYear A vector of extraction rates (\%) per year of the simulation, starting at the second year. If specified,
#'                   the annual demand by species will be applied for the first year of the simulation, but it will be rescaled for the remaining
#'                   years according to the growth observed and the desired extraction rates.
#' @param managementArguments A list of arguments to be passed to the managementFunction. These arguments will be taken as defaults 
#'                            copied for all management units and can later be modified. If NULL, the result of calling function 
#'                            \code{\link{defaultManagementArguments}} will be taken.
#'
#' @return A list with the following structure:
#'    \itemize{
#'      \item{\code{scenarioType}: Either 'bottom-up' (no demand is specified), 'input_demand' (annual species demand is specified), or 
#'            'input_rate' when extraction rates are also supplied.}
#'      \item{\code{managementUnits}: A vector of length numberOfUnits, each with the supplied management arguments}
#'      \item{\code{volumeDemandBySpecies}: A vector of annual wood demand (m3) by species (for scenarioType 'bottom-up' or 'input_demand'.}
#'      \item{\code{extractionRateByYear}: A vector of extraction rate values per year.}
#'    }
#'
#' @seealso \code{\link{fordyn_scenario}}, \code{\link{defaultManagementFunction}}
#' 
#' @examples 
#' 
#' # A scenario with three management units and annual demand for two species
#' s1 = create_management_scenario(3,  c("Quercus ilex" = 1000, "Pinus nigra" = 2000))
#' 
#' # A scenario like the former, but with total annual demand changing as a function of
#' # prescribed extraction rates (second and third years)
#' s2 = create_management_scenario(3,  
#'         c("Quercus ilex" = 1000, "Pinus nigra" = 2000),
#'         c("2002" = 30, "2003" = 50))
create_management_scenario<-function(numberOfUnits,
                                     annualDemandBySpecies = NULL,
                                     extractionRateByYear = NULL,
                                     managementArguments = NULL) {
  
  scenarioType = "bottom-up"
  if(!is.null(annualDemandBySpecies)) {
    scenarioType = "input_demand"
    if(!is.null(extractionRateByYear)) scenarioType = "input_rate"
  }

  if(is.null(managementArguments)) managementArguments = medfate::defaultManagementArguments()
  
  managementUnits = vector("list", numberOfUnits)
  names(managementUnits) = paste0("Unit_", 1:numberOfUnits)
  for(i in 1:numberOfUnits) {
    mu = list(managementArgs = managementArguments)
    managementUnits[[i]] = mu
  }
  l <- list(scenarioType = scenarioType, 
            annualDemandBySpecies = annualDemandBySpecies,
            extractionRateByYear = extractionRateByYear,
            managementUnits = managementUnits)
  class(l) <- c("management_scenario", "list")
  return(l)
}