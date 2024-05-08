#' Forest imputation
#' 
#' Performs imputation of forest objects from a forest inventory using a forest map to match forest types and topography as covariates. Additional
#' rasters can be supplied to correct forest structure in the target locations.
#'
#' @param x An object of class \code{\link{sf}}. If it contains a column named 'land_cover_type', imputation
#'          will be performed for locations whose land cover is "wildland". Otherwise, forest imputation is done for all locations.
#' @param sf_nfi An object of class \code{\link{sf}} with forest inventory data column 'forest'. 
#' @param dem A digital elevation model (class \code{\link{rast}}) with meters as units
#' @param forest_map An object of class \code{\link{rast}} or \code{\link{vect}} with the forest class map
#' @param height_map An object of class \code{\link{rast}} or \code{\link{vect}} with the forest height map
#' @param basal_area_map An object of class \code{\link{rast}} or \code{\link{vect}} with the forest basal area map
#' @param max_distance_km Maximum distance, in km, for forest inventory plot imputation.
#' @param var_class Variable name or index containing forest classes in 'forest_map'. If missing the first column is taken.
#' @param var_height Variable name or index containing forest height in 'height_map'. If missing the first column is taken.
#' @param var_basal_area Variable name or index containing forest height in 'basal_area_map'. If missing the first column is taken.
#' @param replace_existing A logical flag to force the replacement of existing 'forest' objects, when present.
#' @param empty_forests A logical flag to place empty 'forest' objects for wildland cells not having a defined forest class.
#' @param correct_height_shrubs A logical flag to correct the height of shrubs when providing a vegetation height map (by default, only tree heights are corrected).
#' @param merge_trees A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link{forest_mergeTrees}}).
#' @param merge_shrubs A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link{forest_mergeShrubs}}).
#' @param verbose A logical flag to print console output.
#'
#' @details
#' The function performs a simplistic imputation of forest inventory plots on target locations provided that 
#' they correspond to the same forest class, defined in the forest map, and are within some distance of the target location. 
#' Among the multiple stands that can have the target forest class, the function chooses the one that has the most similar elevation 
#' and position in the N-to-S slopes (i.e. the product of the cosine of aspect and slope). Both topographic 
#' features are standardized to zero mean and unit standard deviation, to make their influence on the imputation
#' equal. This imputation method will be more or less successful depending on the resolution of forest classes and
#' the number of forest inventory plots available for each of them.
#' 
#' When \code{height_map} or \code{basal_area_map} are provided, the function performs structural corrections in additional to imputation.
#' When correcting for height, the function does not force average tree height of the stand in the target location to match the height map. 
#' Rather, it uses the ratio of heights between the target location and the forest inventory plot used as imputation source, to correct the 
#' tree heights of the target location. Tree diameters are corrected with the same factor, assuming that the diameter-height relationship 
#' does not depend on tree height. This leads consequently in a change in basal area, in addition to changes in dominant tree height. 
#' 
#' Correction for basal area operates on tree density. Analogously to the correction of heights, it does not take the basal area value of the map 
#' for the target location, but the ratio of values between the target location and the forest inventory plot used as reference for imputation. 
#' This ratio is used to correct tree density.
#' 
#' @return A modified object of class \code{\link{sf}} with column 'forest'.
#' @seealso [create_landscape()], [add_soilgrids()], \code{\link[medfate]{forest_mergeTrees}}
#' @export
#'
#' @examples
#' \dontrun{
#'   # See vignette 'Landscape inputs'
#' }
impute_forests <-function(x, sf_nfi, dem, 
                          forest_map, height_map = NULL, basal_area_map = NULL, 
                          var_class = NA, var_height = NA, var_basal_area = NA,
                          max_distance_km = 50,
                          replace_existing = FALSE, empty_forests = TRUE, 
                          correct_height_shrubs = FALSE,
                          merge_trees = TRUE, merge_shrubs = TRUE, verbose = TRUE) {
  if(verbose) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) stop("'x' should be of class 'sf' ")
  land_cover_type <- rep("wildland", nrow(x))
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  if(!inherits(dem, "SpatRaster")) stop("'dem' should be of class 'SpatRaster'")
  if(!inherits(forest_map, "SpatRaster") && !inherits(forest_map, "SpatVector")) stop("'forest_map' should be of class 'SpatRaster' or 'SpatVector'")
  if(!is.null(height_map)) {
    if(!inherits(height_map, "SpatRaster") && !inherits(height_map, "SpatVector")) stop("'height_map' should be of class 'SpatRaster' or 'SpatVector'")
  }
  if(!is.null(basal_area_map)) {
    if(!inherits(basal_area_map, "SpatRaster") && !inherits(basal_area_map, "SpatVector")) stop("'basal_area_map' should be of class 'SpatRaster' or 'SpatVector'")
  }
  if(is.na(var_class)) var_class = 1
  if(is.na(var_height)) var_height = 1
  if(is.na(var_basal_area)) var_basal_area = 1
  
  if(verbose) cli::cli_progress_step("Calculating northing-slope")
  r_slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  r_aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  r_northing <- r_slope*cos(pi*r_aspect/180)
  rm(r_slope)
  rm(r_aspect)
  gc()
  if(verbose) cli::cli_progress_step("Calculating topography mean and sd values")
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
  x_class<-terra::extract(forest_map, x_vect)[,-1, drop = FALSE]
  x_class<- x_class[[var_class]]
  classes <- unique(x_class[!is.na(x_class)])
  forest_map_red <- forest_map[forest_map$Class %in% classes,]
  if(verbose) cli::cli_progress_step("Extracting forest class for 'sf_nfi'")
  nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(forest_map)))
  nfi_class<-terra::extract(forest_map_red, nfi_vect)[,-1, drop = FALSE]
  nfi_class<- nfi_class[[var_class]]
  if(!is.null(height_map)) {
    if(verbose) cli::cli_progress_step("Extracting height for 'x'")
    x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(height_map)))
    x_height<-terra::extract(height_map, x_vect)[,-1, drop = FALSE]
    x_height<- x_height[[var_height]]
    if(verbose) cli::cli_progress_step("Extracting height for 'sf_nfi'")
    nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(height_map)))
    nfi_height<-terra::extract(height_map, nfi_vect)[,-1, drop = FALSE]
    nfi_height<-nfi_height[[var_height]]
  }
  if(!is.null(basal_area_map)) {
    if(verbose) cli::cli_progress_step("Extracting basal area for 'x'")
    x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(basal_area_map)))
    x_basal_area<-terra::extract(basal_area_map, x_vect)[,-1, drop = FALSE]
    x_basal_area<- x_basal_area[[var_basal_area]]
    if(verbose) cli::cli_progress_step("Extracting basal area for 'sf_nfi'")
    nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(basal_area_map)))
    nfi_basal_area<-terra::extract(basal_area_map, nfi_vect)[,-1, drop = FALSE]
    nfi_basal_area<-nfi_basal_area[[var_basal_area]]
  }
  if(verbose) cli::cli_progress_step("Equidistant conic coordinates")
  x_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(x), crs = "ESRI:54027"))
  nfi_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(sf_nfi), crs = "ESRI:54027"))
  
  if(!("forest" %in% names(x))) {
    if(verbose) cli::cli_progress_step("Defining column 'forest'")
    x$forest <- vector("list", nrow(x))
  }
  if(verbose) {
    cli::cli_progress_step("Imputation")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  for(i in 1:nrow(x)) {
    if(verbose) cli::cli_progress_update()
    if(x$land_cover_type[i]=="wildland") {
      if(!is.na(x_class[i])) {
        nfi_sel <- nfi_class==x_class[i]
        nfi_sel[is.na(nfi_sel)] <- FALSE
        if(sum(nfi_sel)==0) stop("Could not find plots of the same forest class. Revise inputs.")
        if(!is.null(max_distance_km)) {
          if(!is.na(max_distance_km)) {
            cc_1 <- x_equi_cc[i,1] - nfi_equi_cc[,1] 
            cc_2 <- x_equi_cc[i,2] - nfi_equi_cc[,2] 
            d_km <- sqrt(cc_1^2 + cc_2^2)/1000
            nfi_sel[d_km > max_distance_km] <- FALSE
            if(sum(nfi_sel)==0) stop("Not enough plots within geographic distance limits. Increase 'max_distance_km'.")
          }
        }
        nfi_w <- which(nfi_sel)
        y_1 <- x_m[i,1] - nfi_m[nfi_w,1] 
        y_2 <- x_m[i,2] - nfi_m[nfi_w,2] 
        nfi_i <- nfi_w[which.min(y_1^2 + y_2^2)]
        f <- sf_nfi$forest[[nfi_i]]
        height_ratio <- 1
        if(!is.null(height_map)) {
          height_ratio <- nfi_height[nfi_i]/x_height[i]
        }
        basal_area_ratio <- 1
        if(!is.null(basal_area_map)) {
          basal_area_ratio <- nfi_basal_area[nfi_i]/x_basal_area[i]
        }
        if(is.null(x$forest[[i]]) || replace_existing) {
          if(!is.na(height_ratio)) {
            if(height_ratio!=1) {
              if(nrow(f$treeData)>0) {
                f$treeData$Height <- f$treeData$Height*height_ratio 
                f$treeData$DBH <- f$treeData$DBH*height_ratio
              }
              if(correct_height_shrubs) {
                if(nrow(f$shrubData)>0) {
                  f$shrubData$Height <- f$shrubData$Height*height_ratio 
                }
              }
            }
          }
          if(!is.na(basal_area_ratio)) {
            if(basal_area_ratio!=1) {
              if(nrow(f$treeData)>0) {
                f$treeData$N <- f$treeData$N*basal_area_ratio
              }
            }
          }
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