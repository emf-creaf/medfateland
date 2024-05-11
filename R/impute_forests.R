#' Landscape forest parametrization
#' 
#' Function \code{impute_forests()} performs imputation of forest objects from a forest inventory using a forest map to match forest types and topography as covariates. 
#' Function \code{modify_forest_structure()} uses forest structure rasters supplied by the user to correct forest structure metrics.
#'
#' @param x An object of class \code{\link{sf}}. If it contains a column named 'land_cover_type', imputation
#'          will be performed for locations whose land cover is "wildland". Otherwise, forest imputation is done for all locations.
#'          For structural corrections, \code{x} should already contain a column named 'forest' containing  \code{\link{forest}} objects.
#' @param sf_nfi An object of class \code{\link{sf}} with forest inventory data column 'forest'. 
#' @param dem A digital elevation model (class \code{\link{rast}}) with meters as units
#' @param forest_map An object of class \code{\link{rast}} or \code{\link{vect}} with the forest class map
#' @param max_distance_km Maximum distance, in km, for forest inventory plot imputation.
#' @param var_class Variable name or index containing forest classes in 'forest_map'. If missing the first column is taken.
#' @param replace_existing A logical flag to force the replacement of existing \code{\link{forest}} objects, when present.
#' @param merge_trees A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link{forest_mergeTrees}}).
#' @param merge_shrubs A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link{forest_mergeShrubs}}).
#' @param progress A logical flag to print console output.
#'
#' @details
#' Function \code{impute_forests()} performs imputation of forest inventory plots on target locations provided that 
#' they correspond to the same forest class, defined in the input forest map, and are geographically closer than a distance threshold (\code{max_distance_km}). 
#' Among the multiple stands that can have fulfill these two requirements, the function chooses the one that has the most similar elevation 
#' and position in the N-to-S slopes (i.e. the product of the cosine of aspect and slope). Both topographic 
#' features are standardized to zero mean and unit standard deviation (using the supplied digital elevation model to calculate those metrics), to make their 
#' weight on the imputation equal. This imputation method will be more or less successful depending on the resolution of forest classes and
#' the number of forest inventory plots available for each of them. Additionally, tree and shrub cohorts can be simplified after imputation (\code{merge_trees} and \code{merge_shrubs}), 
#' to reduce the number of records (and hence, speed-up simulations).
#' 
#' Function \code{modify_forest_structure} can be used to modify specific structure variables of the imputed forests 
#' building on rasters supplied by the user (typically from aerial or satellite LiDAR products). For any given metric,
#' the function will calculate the ratio of the structure metric between the target \code{\link{forest}} object (see \code{\link[medfate]{stand_basalArea}}) 
#' and the input map in the target location. Locations where the metric value in the map is missing are left unmodified. 
#' Options for structural variables are the following:
#' \itemize{
#'   \item{\code{mean_tree_height}: Should contain values in cm. Corrects tree heights and diameters (assuming a constant diameter-height relationship).}
#'   \item{\code{dominant_tree_height}: Should contain values in cm. Corrects tree heights and diameters (assuming a constant diameter-height relationship).}
#'   \item{\code{tree_density}: Should contain values in individuals per hectare. Corrects tree density.}
#'   \item{\code{basal_area}: Should contain values in squared meters per hectare (m2/ha). Corrects tree density.}
#'   \item{\code{mean_shrub_height}: Should contain values in cm. Corrects shrub cover.}
#' }
#' 
#' 
#' @return Both functions return a modified object of class \code{\link{sf}}. 
#' 
#' @seealso [create_landscape()], [add_soilgrids()], \code{\link[medfate]{forest_mergeTrees}}
#' @export
#' @name forest_parametrization
#' @examples
#' \dontrun{
#'   # See vignette 'Preparing inputs'
#' }
impute_forests <-function(x, sf_nfi, dem, 
                          forest_map, 
                          var_class = NA, 
                          max_distance_km = 100,
                          replace_existing = FALSE, 
                          merge_trees = TRUE, merge_shrubs = TRUE, progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  land_cover_type <- rep("wildland", nrow(x))
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  if(!inherits(dem, "SpatRaster")) cli::cli_abort("'dem' should be of class 'SpatRaster'")
  if(!inherits(forest_map, "SpatRaster") && !inherits(forest_map, "SpatVector")) cli::cli_abort("'forest_map' should be of class 'SpatRaster' or 'SpatVector'")
  if(is.na(var_class)) var_class = 1

  if(progress) cli::cli_progress_step("Calculating northing-slope")
  r_slope <- terra::terrain(dem, v = "slope", unit = "degrees")
  r_aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")
  r_northing <- r_slope*cos(pi*r_aspect/180)
  rm(r_slope)
  rm(r_aspect)
  gc()
  if(progress) cli::cli_progress_step("Calculating topography mean and sd values")
  mean_elev <- mean(as.vector(dem), na.rm=TRUE)
  sd_elev <- sd(as.vector(dem), na.rm=TRUE)
  mean_northing <- mean(as.vector(r_northing), na.rm=TRUE)
  sd_northing <- sd(as.vector(r_northing), na.rm=TRUE)

  if(progress) cli::cli_progress_step("Extracting topography for 'x'")
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(dem)))
  x_elevation <- terra::extract(dem, x_vect)[,2]
  x_northing <- terra::extract(r_northing, x_vect)[,2]
  x_m <- cbind((x_elevation - mean_elev)/sd_elev, (x_northing - mean_northing)/sd_northing)
  if(progress) cli::cli_progress_step("Extracting topography for 'sf_nfi'")
  nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(dem)))
  nfi_elevation <- terra::extract(dem, nfi_vect)[,2]
  nfi_northing <- terra::extract(r_northing, nfi_vect)[,2]
  nfi_m <- cbind((nfi_elevation - mean_elev)/sd_elev, (nfi_northing - mean_northing)/sd_northing)
  if(progress) cli::cli_progress_step("Extracting forest class for 'x'")
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(forest_map)))
  # Subset map to accelerate extraction
  forest_map_red <- terra::crop(forest_map, x_vect, ext = TRUE)
  x_class<-terra::extract(forest_map_red, x_vect)[,-1, drop = FALSE]
  x_class<- x_class[[var_class]]
  classes <- unique(x_class[!is.na(x_class)])
  forest_map_red <- forest_map[forest_map$Class %in% classes,]
  if(progress) cli::cli_progress_step("Extracting forest class for 'sf_nfi'")
  nfi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_nfi), terra::crs(forest_map)))
  nfi_class<-terra::extract(forest_map_red, nfi_vect)[,-1, drop = FALSE]
  nfi_class<- nfi_class[[var_class]]
  # Check for classes not defined in nfi data  
  non_included = unique(x_class[which(!(x_class %in% nfi_class))])
  if(length(non_included)>0) {
    cli::cli_alert_warning(paste0(length(non_included), " forest classes were not represented in nfi data and the class of ", sum(x_class %in% non_included)," locations was set to missing"))
    x_class[x_class %in% non_included] <- NA
  }
  if(progress) cli::cli_progress_step("Equidistant conic coordinates")
  x_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(x), crs = "ESRI:54027"))
  nfi_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(sf_nfi), crs = "ESRI:54027"))
  
  if(!("forest" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining column 'forest'")
    x$forest <- vector("list", nrow(x))
  }
  if(progress) {
    cli::cli_progress_step("Imputation")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  num_closest <- 0
  num_missing <- 0
  for(i in 1:nrow(x)) {
    if(progress) cli::cli_progress_update()
    if(x$land_cover_type[i]=="wildland") {
      if(!is.na(x_class[i])) {
        nfi_sel <- nfi_class==x_class[i]
        nfi_sel[is.na(nfi_sel)] <- FALSE
        if(sum(nfi_sel)==0) cli::cli_abort("Could not find plots of the same forest class. Revise inputs.")
      } else {
        nfi_sel <- rep(TRUE, length(nfi_class))
        num_missing <- num_missing + 1
      }
      if(!is.null(max_distance_km)) {
        if(!is.na(max_distance_km)) {
          cc_1 <- x_equi_cc[i,1] - nfi_equi_cc[,1] 
          cc_2 <- x_equi_cc[i,2] - nfi_equi_cc[,2] 
          d_km <- sqrt(cc_1^2 + cc_2^2)/1000
          nfi_sel[d_km > max_distance_km] <- FALSE
          if(sum(nfi_sel)==0) {
            num_closest <- num_closest + 1
            nfi_sel[which.min(d_km)] <- TRUE
          }
        }
      }
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
    }
  }
  if(num_missing> 0)  cli::cli_alert_warning(paste0("Missing forest class for ", num_missing, " locations. Only geographic and topographic criteria used for those locations."))
  if(num_closest> 0)  cli::cli_alert_warning(paste0("Not enough plots of the same class within geographic distance limits for ", num_closest, " locations. The closest plot of the same class was chosen in those cases."))
  if(progress) cli::cli_progress_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}



#' @rdname forest_parametrization
#' @param structure_map An object of class \code{\link{rast}} or \code{\link{vect}} with a forest structural variable map
#' @param variable Structural variable to correct. See options in details.
#' @param map_var Variable name or index containing structural variable in 'structure_map'. If missing the first column is taken.
#' @param ratio_limits Limits for ratio of variable in corrections, used to avoid outliers. 
#' @export
modify_forest_structure<-function(x, structure_map, variable,
                                  map_var = NA, 
                                  ratio_limits = NULL,
                                  progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  if(!("forest" %in% names(x))) cli::cli_abort("Column 'forest' must be defined.")
  if(!inherits(structure_map, "SpatRaster") && !inherits(structure_map, "SpatVector")) cli::cli_abort("'structure_map' should be of class 'SpatRaster' or 'SpatVector'")
  if(is.na(map_var)) map_var = 1
  variable <- match.arg(variable, c("mean_tree_height", "dominant_tree_height", "tree_density", "basal_area"))
  
  if(progress) cli::cli_progress_step(paste0("Extracting ", variable))
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(structure_map)))
  x_var<-terra::extract(structure_map, x_vect)[,-1, drop = FALSE]
  x_var<- x_var[[map_var]]
  
  
  if(progress) {
    cli::cli_progress_step("Correction")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  for(i in 1:nrow(x)) {
    if(progress) cli::cli_progress_update()
    f <- x$forest[[i]]
    if((!is.null(f)) && (!is.na(x_var[i]))) {
      if(variable=="mean_tree_height") {
        if(nrow(f$treeData)>0) {
          mean_height_m <- stand_meanTreeHeight(f)
          height_ratio <- x_var[i]/mean_height_m
          if(!is.null(ratio_limits)) height_ratio <- max(min(height_ratio, ratio_limits[2]), ratio_limits[1])
          f$treeData$Height <- f$treeData$Height*height_ratio 
          f$treeData$DBH <- f$treeData$DBH*height_ratio
        }
      } else if(variable=="dominant_tree_height") {
        if(nrow(f$treeData)>0) {
          dominant_height_m <- stand_dominantTreeHeight(f)
          height_ratio <- x_var[i]/dominant_height_m
          if(!is.null(ratio_limits)) height_ratio <- max(min(height_ratio, ratio_limits[2]), ratio_limits[1])
          f$treeData$Height <- f$treeData$Height*height_ratio 
          f$treeData$DBH <- f$treeData$DBH*height_ratio
        }
      } else if(variable=="tree_density") {
        if(nrow(f$treeData)>0) {
          tree_density <- stand_treeDensity(f)
          density_ratio <- x_var[i]/tree_density
          if(!is.null(ratio_limits)) density_ratio <- max(min(density_ratio, ratio_limits[2]), ratio_limits[1])
          f$treeData$N <- f$treeData$N*density_ratio
        }
      } else if(variable=="basal_area") {
        if(nrow(f$treeData)>0) {
          basal_area <- stand_basalArea(f)
          basal_area_ratio <- x_var[i]/basal_area
          if(!is.null(ratio_limits)) basal_area_ratio <- max(min(basal_area_ratio, ratio_limits[2]), ratio_limits[1])
          f$treeData$N <- f$treeData$N*basal_area_ratio
        }
      }
      x$forest[[i]] <- f
    }
  }
  if(progress) {
    cli::cli_progress_done()
  }
  return(sf::st_as_sf(tibble::as_tibble(x)))
}