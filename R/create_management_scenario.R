#' Create management scenario
#' 
#' Defines a management scenario for simulations of forest dynamics 
#'
#' @param units Number of management units. Alternatively, a data frame with management options (in columns) for a set of units (in rows). Options not specified witl be taken from defaults.
#' @param annual_demand_by_species A vector or matrix of annual wood demand (m3) by medfate species names (or groups of species names). If empty, the scenario is 'bottom-up' (not based on demand).
#'                   If a vector is supplied, the same wood demand is applied for all simulated years. If a matrix is supplied, each row should correspond
#'                   to a different year.
#' @param extraction_rate_by_year A vector of extraction rates (%) per year of the simulation, starting at the second year. If specified,
#'                   the annual demand by species will be applied for the first year of the simulation, but it will be rescaled for the remaining
#'                   years according to the growth observed and the desired extraction rates.
#' @param default_management_arguments A list of arguments to be passed to the managementFunction. These arguments will be taken as defaults 
#'                            copied for all management units and can later be modified. If NULL, the result of calling function 
#'                            \code{\link[medfate]{defaultManagementArguments}} will be taken.
#'
#' @details 
#' Three kinds of management scenarios are allowed:
#' \enumerate{
#'   \item{\code{'bottom-up'} represents a scenario where forest stands belong to different management units, 
#'   each of them having possibly distinct management prescriptions. However, there is no demand and the amount
#'   of extracted wood emerges from the interplay between forest dynamics and management prescriptions.}
#'   \item{\code{'input_demand'} represents a scenario where a certain amount of wood extraction is targeted for some species
#'   and each year. This requires deciding which stands will actually undergo thinning operations to fulfill the demand (stands 
#'   managed following prescriptions that indicate final regeneration cuts are managed irrespective of demand).}
#'   \item{\code{'input_rate'} represents a scenario similar to the previous one but where total amount of wood targeted depends
#'   on (i.e. is a proportion of) the growth observed in previous year.}
#' }
#' 
#' The kind of management scenario depends on the arguments supplied by the user when calling \code{create_management_scenario} (see examples). 
#' In all cases, management units need to be defined. Each management unit represents a group of forest stands 
#' following the same management prescriptions. Although the \code{create_management_scenario} function allows specifying the management arguments of each unit,
#' the simulation of management scenarios also requires specifying, for each forest stand, to which management unit it belongs (see \code{\link{fordyn_scenario}}).
#' 
#' @return A list with the following structure:
#'    \itemize{
#'      \item{\code{scenario_type}: Either 'bottom-up' (no demand is specified), 
#'            'input_demand' (annual species demand is specified), or 
#'            'input_rate' when extraction rates are also supplied.}
#'      \item{\code{annual_demand_by_species}: A vector of annual wood demand (m3) by species (or species groups) 
#'            (for scenario_type 'bottom-up' or 'input_demand').}
#'      \item{\code{extraction_rate_by_year}: A vector of extraction rate values per year.}
#'      \item{\code{units}: A data frame with as many rows as units and management arguments as columns.}
#'    }
#'
#' @author 
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Aitor \enc{Améztegui}{Ameztegui}, UdL
#' 
#' @seealso \code{\link{fordyn_scenario}}, \code{\link[medfate]{defaultManagementFunction}}, \code{\link{defaultPrescriptionsBySpecies}}, \code{\link{create_fire_regime}}
#' 
#' @examples 
#' 
#' # A scenario with three management units and annual demand for two species
#' scen_1 <- create_management_scenario(3,  c("Quercus ilex" = 1000, "Pinus nigra" = 2000))
#' 
#' # A scenario like the former, but with total annual demand changing as a function of
#' # prescribed extraction rates (second and third years)
#' scen_2 <- create_management_scenario(3,  
#'            c("Quercus ilex" = 1000, "Pinus nigra" = 2000),
#'            c("2002" = 30, "2003" = 50))
#'         
#' # A scenario with as many management units as rows in 'defaultPrescriptionsBySpecies'
#' # and not based on demand
#' data("defaultPrescriptionsBySpecies")
#' scen_3 <- create_management_scenario(defaultPrescriptionsBySpecies)
#' 
#' # A scenario with three management units and annual demand for one species group 
#' # and a third species
#' scen_4 <- create_management_scenario(3,  c("Quercus ilex/Quercus pubescens" = 1000, 
#'                                            "Pinus nigra" = 2000))
#' @export
create_management_scenario<-function(units,
                                     annual_demand_by_species = NULL,
                                     extraction_rate_by_year = NULL,
                                     default_management_arguments = NULL) {
  
  if(is.null(default_management_arguments)) default_management_arguments = medfate::defaultManagementArguments()
  arg_names = names(default_management_arguments)
  if(is.numeric(units)) {
    num_units = units
    unit_df = data.frame(row.names = paste0("unit_", 1:num_units))
    for(i in 1:num_units) {
      for(j in 1:length(arg_names)) unit_df[i, arg_names[j]] = default_management_arguments[[j]]
    }
  } else if(is.data.frame(units)) {
    df = units
    df_names = names(df)[names(df) %in% arg_names]
    if(!("Name" %in% names(df))) stop("Column 'Name' must be specified in data frame")
    unit_df = data.frame(row.names = df$Name)
    for(i in 1:nrow(df)) {
      args = default_management_arguments
      args[df_names] = as.list(df[i, df_names]) 
      for(j in 1:length(arg_names)) unit_df[i, arg_names[j]] = args[[j]]
    }
  }
  scenario_type = "bottom-up"
  if(!is.null(annual_demand_by_species)) {
    if(!is.numeric(annual_demand_by_species)) stop("Annual demand by species should be a named numeric vector")
    target_taxon_names <- names(annual_demand_by_species) # Species or species groups
    if(is.null(target_taxon_names)) stop("Annual demand vector should named")
    target_spp_names <- unlist(strsplit(target_taxon_names, "/")) # Vector of species names mentioned
    if(length(target_spp_names) > length(unique(target_spp_names))) {
      stop("Demand species names cannot be repeated!")
    }
    scenario_type = "input_demand"
    if(!is.null(extraction_rate_by_year)) {
      if(!is.numeric(extraction_rate_by_year)) stop("Extraction rates should be a named numeric vector")
      if(is.null(names(extraction_rate_by_year))) stop("Extraction rates should be named")
      scenario_type = "input_rate"
    }
  }
  l <- list(scenario_type = scenario_type, 
            annual_demand_by_species = annual_demand_by_species,
            extraction_rate_by_year = extraction_rate_by_year,
            units = unit_df)
  class(l) <- c("management_scenario", "list")
  return(l)
}
