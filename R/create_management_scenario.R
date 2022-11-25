#' Create management scenario
#' 
#' Defines a management scenario with default values, to be modified by the user
#'
#' @param units Number of management units. Each management unit represents a group of forest stands following the same silvicultural model.
#' @param annual_demand_by_species A vector or matrix of annual wood demand (m3) by medfate species names. If empty, the scenario is 'bottom-up' (not based on demand).
#'                   If a vector is supplied, the same wood demand is applied for all simulated years. If a matrix is supplied, each row should correspond
#'                   to a different year.
#' @param extraction_rate_by_year A vector of extraction rates (%) per year of the simulation, starting at the second year. If specified,
#'                   the annual demand by species will be applied for the first year of the simulation, but it will be rescaled for the remaining
#'                   years according to the growth observed and the desired extraction rates.
#' @param default_management_arguments A list of arguments to be passed to the managementFunction. These arguments will be taken as defaults 
#'                            copied for all management units and can later be modified. If NULL, the result of calling function 
#'                            \code{\link{defaultManagementArguments}} will be taken.
#'
#' @return A list with the following structure:
#'    \itemize{
#'      \item{\code{scenario_type}: Either 'bottom-up' (no demand is specified), 
#'            'input_demand' (annual species demand is specified), or 
#'            'input_rate' when extraction rates are also supplied.}
#'      \item{\code{units}: A named list of length the number of units, where each element has management arguments}
#'      \item{\code{annual_demand_by_species}: A vector of annual wood demand (m3) by species 
#'            (for scenario_type 'bottom-up' or 'input_demand').}
#'      \item{\code{extraction_rate_by_year}: A vector of extraction rate values per year.}
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
create_management_scenario<-function(units,
                                     annual_demand_by_species = NULL,
                                     extraction_rate_by_year = NULL,
                                     default_management_arguments = NULL) {
  
  if(is.integer(units)) {
    num_units = units
    unit_list = vector("list", num_units)
    names(unit_list) = paste0("unit_", 1:num_units)
    for(i in 1:num_units) {
      unit_list[[i]] = default_management_arguments
    }
    if(is.null(default_management_arguments)) default_management_arguments = medfate::defaultManagementArguments()
  } else if(is.data.frame(units)) {
    df = units
    num_units = nrow(df)
    unit_list = vector("list", num_units)
    names(unit_list) = df$Name
    
  }
  scenario_type = "bottom-up"
  if(!is.null(annual_demand_by_species)) {
    scenario_type = "input_demand"
    if(!is.null(extraction_rate_by_year)) scenario_type = "input_rate"
  }
  l <- list(scenario_type = scenario_type, 
            annual_demand_by_species = annual_demand_by_species,
            extraction_rate_by_year = extraction_rate_by_year,
            units = unit_list)
  class(l) <- c("management_scenario", "list")
  return(l)
}
