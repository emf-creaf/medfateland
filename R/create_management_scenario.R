#' Create management scenario
#' 
#' Defines a management scenario with default values, to be modified by the user
#'
#' @param numberOfUnits Number of management units. Each management unit represents a group of forest stands following the same silvicultural model.
#' @param managementFunction A function that implements forest management actions (see \code{\link{fordyn}}). If NULL, function \code{\link{defaultManagementFunction}} will be taken.
#' @param managementArguments A list of arguments to be passed to the managementFunction. These arguments will be taken as defaults to be copied for all management units and can later be modified. 
#'   If NULL, the result of calling function \code{\link{defaultManagementArguments}} will be taken.
#'
#' @return A list with the following structure:
#'    \itemize{
#'      \item{\code{managementFunction}: The supplied management function}
#'      \item{\code{managementUnits}: A vector of length numberOfUnits, each with the supplied management arguments}
#'      \item{\code{annualDemandBySpecies}: An empty (named) vector of annual wood demand (m3) by medfate species names}
#'    }
#'
#' @examples
#'  # Creates default scenario with two management units
#'  s = create_management_scenario(2)
create_management_scenario<-function(numberOfUnits,
                                     managementFunction = NULL, 
                                     managementArguments = NULL) {
  
  if(is.null(managementFunction)) managementFunction = medfate::defaultManagementFunction
  if(is.null(managementArguments)) managementArguments = medfate::defaultManagementArguments()
  
  managementUnits = vector("list", numberOfUnits)
  names(managementUnits) = paste0("Unit_", 1:numberOfUnits)
  for(i in 1:numberOfUnits) {
    mu = list(managementArgs = managementArguments)
    managementUnits[[i]] = mu
  }
  l <- list(managementFunction = managementFunction,
            managementUnits = managementUnits,
            annualDemandBySpecies = numeric(0))
  class(l) <- c("management_scenario", "list")
  return(l)
}