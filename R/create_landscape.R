#' Creates a landscape
#'
#' Initializes topography and land cover type for a set of target locations
#'
#' @param x An object of class \code{\link{sf}}
#' @param dem A digital elevation model (class \code{\link{SpatRaster}}) with meters as units
#' @param land_cover_map An object of class \code{\link{SpatRaster}} of land cover type. If missing, all locations are considered 'wildland'.
#' @param wildland,agriculture,rock,artificial,water Strings indicating the mapping from the legend of land_cover_map. 
#' @param verbose A logical flag to print console output
#'
#' @details
#' The user should manually define the mapping of land cover classes in \code{land_cover_map} to the land cover types 
#' used in medfateland.
#' 
#' @return A modified object of class \code{\link{sf}} with columns:
#'        \itemize{
#'           \item{\code{elevation}: Elevation above sea level (in m).}
#'           \item{\code{slope}: Slope (in degrees).}
#'           \item{\code{aspect}: Aspect (in degrees).}
#'           \item{\code{land_cover_type}: Land cover type.}
#'        }
#'        
#' @seealso [impute_forests()], [add_soilgrids()]
#' @export
#'
#' @examples
#' \dontrun{
#'   # See vignette 'Preparing inputs'
#' }
create_landscape<-function(x, dem, land_cover_map = NULL, 
                           wildland = NULL, agriculture = NULL, rock = NULL, artificial = NULL, water = NULL,
                           verbose = TRUE) {
  if(verbose) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  if(!inherits(dem, "SpatRaster")) cli::cli_abort("'dem' should be of class 'SpatRaster'")
  if(!is.null(land_cover_map)) if(!inherits(dem, "SpatRaster")) cli::cli_abort("'dem' should be of class 'SpatRaster'")
  
  if(!("id" %in% names(x))) {
    if(verbose) cli::cli_progress_step("Defining column 'id'")
    x$id <- 1:nrow(x)
  }
  if(verbose) cli::cli_progress_step("Extracting topography")
  x_vect <- terra::vect(sf::st_transform(x, terra::crs(dem)))
  x$elevation <- terra::extract(dem, x_vect)[,2]
  slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  x$slope <- terra::extract(slope, x_vect)[,2]
  aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  x$aspect <- terra::extract(aspect, x_vect)[,2]
  if(!is.null(land_cover_map)) {
    if(verbose) cli::cli_progress_step("Extracting land cover")
    x_lcm <- terra::vect(sf::st_transform(x, terra::crs(land_cover_map)))
    x$land_cover_type <- NA
    lct <- terra::extract(land_cover_map, x_lcm)[,2]
    if(!is.null(wildland)) x$land_cover_type[lct %in% wildland] <- "wildland"
    if(!is.null(agriculture)) x$land_cover_type[lct %in% agriculture] <- "agriculture"
    if(!is.null(rock)) x$land_cover_type[lct %in% rock] <- "rock"
    if(!is.null(artificial)) x$land_cover_type[lct %in% artificial] <- "artificial"
    if(!is.null(water))  x$land_cover_type[lct %in% water] <- "water"
  } else {
    x$land_cover_type <- "wildland"
  }
  if(verbose) cli::cli_progress_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}
