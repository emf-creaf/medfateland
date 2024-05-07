#' Forest imputation
#' 
#' Performs imputation of forest objects from a forest inventory using a forest class map to match composition and topography as covariates.
#'
#' @param x An object of class \code{\link{sf}}
#' @param sf_nfi An object of class \code{\link{sf}} with forest inventory data column 'forest'
#' @param dem A digital elevation model (class \code{\link{rast}}) with meters as units
#' @param forest_map An object of class \code{\link{rast}} or \code{\link{vect}} with the forest class map
#' @param varClass Variable name or index containing forest classes in 'forest_map'
#' @param replace_existing A logical flag to force the replacement of existing 'forest' objects, when present
#' @param empty_forests A logical flag to place empty 'forest' objects for wildland cells not having a forest class
#' @param merge_trees A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link{forest_mergeTrees}})
#' @param merge_shrubs A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link{forest_mergeShrubs}})
#' @param verbose A logical flag to print console output.
#'
#' @return A modified object of class \code{\link{sf}} with column 'forest'.
#' @export
#'
#' @examples
#' \dontrun{
#'   # See vignette 'Landscape inputs'
#' }
impute_forests <-function(x, sf_nfi, dem, forest_map, varClass, 
                          replace_existing = FALSE, empty_forests = TRUE, 
                          merge_trees = TRUE, merge_shrubs = TRUE, verbose = TRUE) {
  if(verbose) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  if(!("land_cover_type" %in% names(x))) stop("'x' should have a colummn called 'land_cover_type'")
  if(!inherits(dem, "SpatRaster")) stop("'dem' should be of class 'SpatRaster'")
  if(!inherits(forest_map, "SpatRaster") && !inherits(forest_map, "SpatVector")) stop("'forest_map' should be of class 'SpatRaster' or 'SpatVector'")
  
  if(verbose) cli::cli_progress_step("Calculating northing-slope")
  r_slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  r_aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  r_northing <- r_slope*cos(pi*r_aspect/180)
  rm(r_slope)
  rm(r_aspect)
  gc()
  if(verbose) cli::cli_progress_step("Calculating mean/sd topography")
  mean_elev <- mean(as.vector(dem), na.rm=TRUE)
  sd_elev <- sd(as.vector(dem), na.rm=TRUE)
  mean_northing <- mean(as.vector(r_northing), na.rm=TRUE)
  sd_northing <- sd(as.vector(r_northing), na.rm=TRUE)
  if(verbose) cli::cli_progress_step("Extracting topography for 'x'")
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(dem)))
  x_elevation <- terra::extract(dem, x_vect)[,2]
  x_northing <- terra::extract(r_northing, x_vect)[,2]
  x_m <- cbind((x_elevation - mean_elev)/sd_elev, (x_northing - mean_northing)/sd_northing)
  if(verbose) cli::cli_progress_step("Extracting topography for 'sf_nfi'")
  nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(dem)))
  nfi_elevation <- terra::extract(dem, nfi_vect)[,2]
  nfi_northing <- terra::extract(r_northing, nfi_vect)[,2]
  nfi_m <- cbind((nfi_elevation - mean_elev)/sd_elev, (nfi_northing - mean_northing)/sd_northing)
  if(verbose) cli::cli_progress_step("Extracting forest class for 'x'")
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(forest_map)))
  x_class<-terra::extract(forest_map, x_vect)[[varClass]]
  classes <- unique(x_class[!is.na(x_class)])
  forest_map_red <- forest_map[forest_map$Class %in% classes,]
  if(verbose) cli::cli_progress_step("Extracting forest class for 'sf_nfi'")
  nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(forest_map)))
  nfi_class<-terra::extract(forest_map_red, nfi_vect)[[varClass]]
  if(!("forest" %in% names(x))) {
    if(verbose) cli::cli_progress_step("Defining column 'forest'")
    x$forest <- vector("list", nrow(x))
  }
  if(verbose) cli::cli_progress_step("Imputation")
  for(i in 1:nrow(x)) {
    if(x$land_cover_type[i]=="wildland") {
      if(!is.na(x_class[i])) {
        nfi_sel <- nfi_class==x_class[i]
        nfi_sel[is.na(nfi_sel)] <- FALSE
        nfi_w <- which(nfi_sel)
        y_1 <- x_m[i,1] - nfi_m[nfi_w,1] 
        y_2 <- x_m[i,2] - nfi_m[nfi_w,2] 
        nfi_i <- nfi_w[which.min(y_1^2 + y_2^2)]
        f <- sf_nfi$forest[[nfi_i]]
        if(is.null(x$forest[[i]]) || replace_existing) {
          if(merge_trees)  f <- medfate::forest_mergeTrees(f)
          if(merge_shrubs)  f <- medfate::forest_mergeShrubs(f)
          x$forest[[i]] <- f
        }
      } else {
        if(empty_forests) {
          x$forest[[i]] <- medfate::emptyforest()
        }
      }
    }
  }
  if(verbose) cli::cli_progress_done()
  return(x)
}