#' medfateland: Mediterranean Landscape Forest Simulation
#'
#' Functions to simulate forest hydrology, forest function and dynamics over landscapes
#'
#' @name medfateland-package
#' @aliases medfateland medfateland-package
#' @docType package
#' @author 
#' Maintainer: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#' 
#' Author: Aitor Améztegui
#' [ORCID](https://orcid.org/0000-0003-2006-1559)
#' 
#' Author: María González
#' [ORCID](https://orcid.org/0000-0002-2227-8404)
#' 
#' Author: Núria Aquilué
#' 
#' Contributor: Mario Beltrán
#' 
#' Contributor: Rodrigo Balaguer-Romano
#' [ORCID](https://orcid.org/0000-0003-2808-6777)
#' 
#' Contributor: Roberto Molowny-Horas
#' [ORCID](https://orcid.org/0000-0003-2626-6379)
#' 
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/medfateland/index.html}} }
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @import medfate
#' @import meteoland
#' @import methods
#' @import sf
#' @import shiny
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_progress_update
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr bind_rows
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom lifecycle deprecated
#' @importFrom parallel clusterApply
#' @importFrom parallel clusterExport
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom Rcpp evalCpp
#' @importFrom rlang duplicate
#' @importFrom stars st_get_dimension_values
#' @importFrom stats rlnorm
#' @importFrom stats rpois
#' @importFrom stats var
#' @importFrom stats sd
#' @importFrom terra cellFromRowCol
#' @importFrom terra cellFromXY
#' @importFrom terra rast
#' @importFrom terra rasterize
#' @importFrom terra terrain
#' @importFrom tidyterra geom_spatraster
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @useDynLib medfateland, .registration = TRUE
## usethis namespace: end
NULL
