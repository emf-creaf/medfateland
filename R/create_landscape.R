#' Creates a landscape
#'
#' Initializes topography and land cover type for a set of target locations
#'
#' @param x An object of class \code{\link{sf}}
#' @param dem A digital elevation model (class \code{\link{rast}}) with meters as units
#' @param land_cover_map An object of class \code{\link{rast}} of land cover type
#' @param wildland,agriculture,rock,artificial,water Strings indicating the mapping from the legend of land_cover_map. 
#'
#' @details
#' Additional details...
#' 
#' @return A modified object of class 'sf' with columns:
#'        \itemize{
#'           \item{\code{elevation}: Elevation above sea level (in m).}
#'           \item{\code{slope}: Slope (in degrees).}
#'           \item{\code{aspect}: Aspect (in degrees).}
#'           \item{\code{land_cover_type}: Land cover type.}
#'        }
#'        
#' @export
#'
#' @examples
#' \dontrun{
#'   # See vignette 'Landscape inputs'
#' }
create_landscape<-function(x, dem, land_cover_map = NULL, 
                           wildland = NULL, agriculture = NULL, rock = NULL, artificial = NULL, water = NULL,
                           progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  x$id <- 1:nrow(x)
  x_vect <- terra::vect(sf::st_transform(x, terra::crs(dem)))
  if(progress) cli::cli_progress_step("Extracting topography")
  x$elevation <- terra::extract(dem, x_vect)[,2]
  slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  x$slope <- terra::extract(slope, x_vect)[,2]
  aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  x$aspect <- terra::extract(aspect, x_vect)[,2]
  if(!is.null(land_cover_map)) {
    if(progress) cli::cli_progress_step("Extracting land cover")
    x_lcm <- terra::vect(sf::st_transform(x, terra::crs(land_cover_map)))
    x$land_cover_type <- NA
    lct <- terra::extract(land_cover_map, x_lcm)[,2]
    if(!is.null(wildland)) x$land_cover_type[lct %in% wildland] <- "wildland"
    if(!is.null(agriculture)) x$land_cover_type[lct %in% agriculture] <- "agriculture"
    if(!is.null(rock)) x$land_cover_type[lct %in% rock] <- "rock"
    if(!is.null(artificial)) x$land_cover_type[lct %in% artificial] <- "artificial"
    if(!is.null(water))  x$land_cover_type[lct %in% water] <- "water"
  }
  if(progress) cli::cli_process_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}
