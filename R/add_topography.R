#' Add topography and land cover
#'
#' Initializes topography and land cover type for a set of target locations
#'
#' @param x An object of class \code{\link[sf]{sf}}
#' @param dem A digital elevation model (class \code{\link[terra]{SpatRaster}}) with meters as units
#' @param land_cover_map An object of class \code{\link[terra]{SpatRaster}} of land cover type. If missing, all locations are considered 'wildland'.
#' @param wildland,agriculture,rock,artificial,water Strings indicating the mapping from the legend of land_cover_map. 
#' @param progress A logical flag to print console output
#'
#' @details
#' The user should manually define the mapping of land cover classes in \code{land_cover_map} to the land cover types 
#' used in medfateland.
#' 
#' @return Function \code{add_topography()} returns a modified object of class \code{\link[sf]{sf}} with columns:
#'        \itemize{
#'           \item{\code{id}: Numeric location identifiers (if not existing).}
#'           \item{\code{elevation}: Elevation above sea level (in m).}
#'           \item{\code{slope}: Slope (in degrees).}
#'           \item{\code{aspect}: Aspect (in degrees).}
#'           \item{\code{land_cover_type}: Land cover type.}
#'        }
#'        Function \code{add_land_cover()} returns a modified object of class \code{\link[sf]{sf}} with new column:
#'        \itemize{
#'           \item{\code{id}: Numeric location identifiers (if not existing).}
#'           \item{\code{land_cover_type}: Land cover type.}
#'        }
#'        
#' @seealso [check_topography()], [check_land_cover()]
#' @export
#' 
#' @name add_topography
#' @examples
#' # See package vignettes 'Preparing inputs'
add_topography<-function(x, dem,
                         progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  if(!inherits(dem, "SpatRaster")) cli::cli_abort("'dem' should be of class 'SpatRaster'")

  if(!("id" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining column 'id'")
    x$id <- 1:nrow(x)
  }
  if(progress) cli::cli_progress_step("Extracting topography")
  x_vect <- terra::vect(sf::st_transform(x, terra::crs(dem)))
  x$elevation <- terra::extract(dem, x_vect)[,2]
  slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  x$slope <- terra::extract(slope, x_vect)[,2]
  aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  x$aspect <- terra::extract(aspect, x_vect)[,2]
  if(progress) cli::cli_progress_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}


#' @export
#' @rdname add_topography
add_land_cover<-function(x, land_cover_map, 
                         wildland = NULL, agriculture = NULL, rock = NULL, artificial = NULL, water = NULL,
                         progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  if(!is.null(land_cover_map)) {
    if(!inherits(land_cover_map, "SpatRaster") && !inherits(land_cover_map, "SpatVector")) cli::cli_abort("'land_cover_map' should be of class 'SpatRaster' or 'SpatVector'")
  }
  if(!("id" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining column 'id'")
    x$id <- 1:nrow(x)
  }
  if(!("land_cover_type" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining column 'land_cover_type'")
    x$land_cover_type <- NA
  }
  if(progress) cli::cli_progress_step("Extracting land cover")
  x_lcm <- terra::vect(sf::st_transform(x, terra::crs(land_cover_map)))
  lct <- terra::extract(land_cover_map, x_lcm)[,2]
  if(!is.null(wildland)) x$land_cover_type[lct %in% wildland] <- "wildland"
  if(!is.null(agriculture)) x$land_cover_type[lct %in% agriculture] <- "agriculture"
  if(!is.null(rock)) x$land_cover_type[lct %in% rock] <- "rock"
  if(!is.null(artificial)) x$land_cover_type[lct %in% artificial] <- "artificial"
  if(!is.null(water))  x$land_cover_type[lct %in% water] <- "water"
  if(progress) cli::cli_progress_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}

