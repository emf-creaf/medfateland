#' Landscape forest parametrization
#' 
#' Utility functions to define forest inputs in a landscape:
#' \itemize{
#'  \item{\code{impute_forests()} performs imputation of forest objects from a forest inventory using a forest map to match forest types and topography as covariates. }
#'  \item{\code{modify_forest_structure()} uses forest structure rasters supplied by the user to correct forest structure metrics.}
#'  \item{\code{check_forests()} checks that forests are defined and do not contain missing values in key tree/shrub attributes.}
#' }
#'
#' @param x An object of class \code{\link[sf]{sf}}. If it contains a column named 'land_cover_type', imputation
#'          will be performed for locations whose land cover is "wildland". Otherwise, forest imputation is done for all locations.
#'          For structural corrections or when checking, \code{x} should already contain a column named 'forest' containing  \code{\link[medfate]{forest}} objects.
#' @param sf_fi An object of class \code{\link[sf]{sf}} with forest inventory data column 'forest'. 
#' @param dem A digital elevation model (class \code{\link[terra]{SpatRaster}}) with meters as units
#' @param forest_map An object of class \code{\link[terra]{SpatRaster}} or \code{\link[terra]{SpatVector}} with the forest class map
#' @param max_distance_km Maximum distance, in km, for forest inventory plot imputation.
#' @param var_class Variable name or index containing forest classes in \code{forest_map}. If missing the first column is taken.
#' @param replace_existing A logical flag to force the replacement of existing \code{\link[medfate]{forest}} objects, when present.
#' @param missing_class_imputation A logical flag to force imputation in locations where forest class is not defined. If \code{missing_class_imputation = TRUE}, imputation in those locations will be based on geographic and topographic criteria only.
#' @param missing_class_forest A \code{\link[medfate]{forest}} object to be used for locations with missing class.
#' @param merge_trees A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link[medfate]{forest_mergeTrees}}).
#' @param merge_shrubs A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link[medfate]{forest_mergeShrubs}}).
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
#' Function \code{modify_forest_structure()} can be used to modify specific structure variables of the imputed forests 
#' building on rasters supplied by the user (typically from aerial or satellite LiDAR products). For any given metric,
#' the function will calculate the ratio of the structure metric between the target \code{\link[medfate]{forest}} object (see \code{\link[medfate]{stand_basalArea}}) 
#' and the input map in the target location. Options for structural variables are the following:
#' \itemize{
#'   \item{\code{mean_tree_height}: Should contain values in cm. Corrects tree heights and diameters (assuming a constant diameter-height relationship).}
#'   \item{\code{dominant_tree_height}: Should contain values in cm. Corrects tree heights and diameters (assuming a constant diameter-height relationship).}
#'   \item{\code{tree_density}: Should contain values in individuals per hectare. Corrects tree density.}
#'   \item{\code{basal_area}: Should contain values in squared meters per hectare (m2/ha). Corrects tree density. Forests that}
#'   \item{\code{mean_shrub_height}: Should contain values in cm. Corrects shrub cover.}
#' }
#' Locations where the metric value in the map is missing are left unmodified. The same happens if metric value is zero, to avoid division by zero. A special case occurs
#' for correction of basal area. In that case, if there are no trees larger than \code{minDBH} but structural map indicates positive values of basal area, 
#' DBH values will be set to minDBH, and correction of basal area will be performed.
#' 
#' @return Functions \code{impute_forests()} and \code{modify_forest_structure()} return a modified object of class \code{\link[sf]{sf}}.
#'  Function \code{check_forests()} returns an invisible data frame with columns indicating missing forest data and missing values in tree or shrub parameters.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @author Rodrigo Balaguer-Romano, CREAF
#' 
#' @seealso [add_topography()], [add_forests()], [add_soilgrids()], \code{\link[medfate]{forest_mergeTrees}}
#' @export
#' @name forest_parametrization
#' @examples
#' # See package vignettes 'Preparing inputs'
impute_forests <-function(x, sf_fi, dem, 
                          forest_map, 
                          var_class = NA, 
                          max_distance_km = 100,
                          replace_existing = FALSE, 
                          missing_class_imputation = FALSE,
                          missing_class_forest = NULL,
                          merge_trees = TRUE, merge_shrubs = TRUE, progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  land_cover_type <- rep("wildland", nrow(x))
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  if(!inherits(dem, "SpatRaster")) cli::cli_abort("'dem' should be of class 'SpatRaster'")
  if(!inherits(forest_map, "SpatRaster") && !inherits(forest_map, "SpatVector")) cli::cli_abort("'forest_map' should be of class 'SpatRaster' or 'SpatVector'")
  if(is.na(var_class)) var_class = 1

  if(!("forest" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining new column 'forest'")
    x$forest <- vector("list", nrow(x))
  }
  # Number of target locations 
  if(replace_existing) {
    is_target <- x$land_cover_type=="wildland"
  } else {
    is_target <- (unlist(lapply(x$forest, is.null)) & x$land_cover_type=="wildland")
  }
  num_target_wildland <- sum(is_target)
  num_closest <- 0
  num_missing <- 0
  num_imputed <- 0 
  
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
  if(any(is.na(x_m))) cli::cli_abort("Missing values in topography for 'x'")
  if(progress) cli::cli_progress_step("Extracting topography for 'sf_fi'")
  fi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_fi), terra::crs(dem)))
  fi_elevation <- terra::extract(dem, fi_vect)[,2]
  fi_northing <- terra::extract(r_northing, fi_vect)[,2]
  fi_m <- cbind((fi_elevation - mean_elev)/sd_elev, (fi_northing - mean_northing)/sd_northing)
  if(any(is.na(fi_m))) cli::cli_abort("Missing values in topography for 'sf_fi'")
  
  if(progress) cli::cli_progress_step("Extracting forest class for 'x'")
  x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(forest_map)))
  # Subset map to accelerate extraction
  forest_map_red <- terra::crop(forest_map, x_vect, ext = TRUE)
  x_class<-terra::extract(forest_map_red, x_vect)[,-1, drop = FALSE]
  x_class<- x_class[[var_class]]
  classes <- unique(x_class[!is.na(x_class)])
  forest_map_red <- forest_map[forest_map$Class %in% classes,]
  if(progress) cli::cli_progress_step("Extracting forest class for 'sf_fi'")
  fi_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_fi), terra::crs(forest_map)))
  fi_class<-terra::extract(forest_map_red, fi_vect)[,-1, drop = FALSE]
  fi_class<- fi_class[[var_class]]
  # Check for classes not defined in fi data  
  non_included <- unique(x_class[which(!(x_class %in% fi_class))])
  # print(non_included)
  if(length(non_included)>0) {
    cli::cli_alert_warning(paste0(length(non_included), " forest classes were not represented in forest inventory data. Geographic/topographic criteria used for ", sum((x_class %in% non_included) & is_target)," target locations."))
    x_class[x_class %in% non_included] <- "no_match_class"
  }
  # print(sum(is.na(x_class) & land_cover_type=="wildland"))
  if(progress) cli::cli_progress_step("Equidistant conic coordinates")
  x_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(x), crs = "ESRI:54027"))
  fi_equi_cc <- sf::st_coordinates(sf::st_transform(sf::st_geometry(sf_fi), crs = "ESRI:54027"))
  

  if(progress) {
    cli::cli_progress_step("Imputation")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  
  for(i in 1:nrow(x)) {
    if(progress) cli::cli_progress_update()
    if(x$land_cover_type[i]=="wildland") {
      if(!is.na(x_class[i])) {
        if(x_class[i]=="no_match_class") {
          fi_sel <- rep(TRUE, length(fi_class))
        } else {
          fi_sel <- fi_class==x_class[i]
          fi_sel[is.na(fi_sel)] <- FALSE
          if(sum(fi_sel)==0) cli::cli_abort("Could not find plots of the same forest class. Revise inputs.")
        }
      } else {
        if(missing_class_imputation) {
          if(!is.null(missing_class_forest)) {
            if(is.null(x$forest[[i]]) || replace_existing) {
              num_imputed <- num_imputed + 1
              x$forest[[i]] <- missing_class_forest
            }
            fi_sel <- rep(FALSE, length(fi_class))
          } else {
            fi_sel <- rep(TRUE, length(fi_class))
          }
        } else {
          fi_sel <- rep(FALSE, length(fi_class))
        }
        if(is.null(x$forest[[i]]) || replace_existing) num_missing <- num_missing + 1
      }
      if(sum(fi_sel)>0) {
        if(!is.null(max_distance_km)) {
          if(!is.na(max_distance_km)) {
            cc_1 <- x_equi_cc[i,1] - fi_equi_cc[,1] 
            cc_2 <- x_equi_cc[i,2] - fi_equi_cc[,2] 
            d_km <- sqrt(cc_1^2 + cc_2^2)/1000
            if(sum((d_km <= max_distance_km) & fi_sel)>0) {# If there are > 0 plots of the right class and closer than the distance, remove the remaining
              fi_sel[d_km > max_distance_km] <- FALSE
            } else { # If not, select the closest plot among those of the right class
              fi_sel[fi_sel] <- (d_km[fi_sel] == min(d_km[fi_sel]))
              num_closest <- num_closest + 1
            }
          }
        }
        fi_w <- which(fi_sel)
        if(length(fi_w)>0) {
          y_1 <- x_m[i,1] - fi_m[fi_w,1] 
          y_2 <- x_m[i,2] - fi_m[fi_w,2] 
          fi_i <- fi_w[which.min(y_1^2 + y_2^2)]
          f <- sf_fi$forest[[fi_i]]
          if(is.null(x$forest[[i]]) || replace_existing) {
            num_imputed <- num_imputed + 1
            if(merge_trees)  f <- medfate::forest_mergeTrees(f)
            if(merge_shrubs)  f <- medfate::forest_mergeShrubs(f)
            x$forest[[i]] <- f
          }
        }
      }
    }
  }
  cli::cli_alert_info(paste0("Forest imputed on ", num_imputed, " out of ", num_target_wildland," target wildland locations (", round(100*num_imputed/num_target_wildland,1),"%)."))
  if(num_missing> 0)  {
    if(missing_class_imputation) {
      cli::cli_alert_info(paste0("Forest class was missing for ", num_missing, " locations and forests were imputed there according to geographic and topographic criteria."))
    } else {
      cli::cli_alert_info(paste0("Forest class was missing for ", num_missing, " locations and forests were not imputed there."))
    }
  }
  if(num_closest> 0)  cli::cli_alert_info(paste0("Not enough plots of the same class within geographic distance limits for ", num_closest, " locations. The closest plot of the same class was chosen in those cases."))
  if(progress) cli::cli_progress_done()
  return(sf::st_as_sf(tibble::as_tibble(x)))
}



#' @rdname forest_parametrization
#' @param structure_map An object of class \code{\link[terra]{SpatRaster}} or \code{\link[terra]{SpatVector}} with a forest structural variable map
#' @param variable Structural variable to correct. See options in details.
#' @param map_var Variable name or index containing structural variable in 'structure_map'. If missing the first column is taken.
#' @param minDBH Minimum diameter for stand metric calculation. If \code{minDBH > 0} then those stands with smaller trees will not be corrected
#' because of the missing stand metric. A special case occurs for correction following basal area (see details).
#' @param ratio_limits Limits for ratio of variable in corrections, used to avoid outliers. 
#' @export
modify_forest_structure<-function(x, structure_map, variable,
                                  map_var = NA, 
                                  ratio_limits = NULL,
                                  minDBH = 7.5,
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
          mean_height_cm <- stand_meanTreeHeight(f, minDBH = minDBH)
          if(!is.na(mean_height_cm)) {
            if(mean_height_cm > 0) {
              height_ratio <- x_var[i]/mean_height_cm
              if(!is.null(ratio_limits)) height_ratio <- max(min(height_ratio, ratio_limits[2]), ratio_limits[1])
              f$treeData$Height <- f$treeData$Height*height_ratio 
              f$treeData$DBH <- f$treeData$DBH*height_ratio
            }
          }
        }
      } else if(variable=="dominant_tree_height") {
        if(nrow(f$treeData)>0) {
          dominant_height_cm <- stand_dominantTreeHeight(f, minDBH = minDBH)
          if(!is.na(dominant_height_cm)) {
            if(dominant_height_cm > 0) {
              height_ratio <- x_var[i]/dominant_height_cm
              if(!is.null(ratio_limits)) height_ratio <- max(min(height_ratio, ratio_limits[2]), ratio_limits[1])
              f$treeData$Height <- f$treeData$Height*height_ratio 
              f$treeData$DBH <- f$treeData$DBH*height_ratio
            }
          }
        }
      } else if(variable=="tree_density") {
        if(nrow(f$treeData)>0) {
          tree_density <- stand_treeDensity(f, minDBH = minDBH)
          if(!is.na(tree_density)) {
            if(tree_density > 0.0) {
              density_ratio <- x_var[i]/tree_density
              if(!is.null(ratio_limits)) density_ratio <- max(min(density_ratio, ratio_limits[2]), ratio_limits[1])
              f$treeData$N <- f$treeData$N*density_ratio
            }
          }
        }
      } else if(variable=="basal_area") {
        if(nrow(f$treeData)>0) {
          basal_area <- stand_basalArea(f, minDBH = minDBH)
          if(!is.na(basal_area)) {
            # If there are no trees >= minDBH but structural map indicates positive basal area, move DBH to minDBH and recalculate basal_area, to avoid division by 0
            if((basal_area == 0.0) && (x_var[i]>0.0)) {
              if(nrow(f$treeData)>0) {
                f$treeData$DBH <- minDBH
                basal_area <- stand_basalArea(f, minDBH = minDBH)
              }
            }
            ## If there are trees >= minDBH in the target plot correct density
            if(basal_area > 0.0) {
              basal_area_ratio <- x_var[i]/basal_area
              if(!is.null(ratio_limits)) basal_area_ratio <- max(min(basal_area_ratio, ratio_limits[2]), ratio_limits[1])
              f$treeData$N <- f$treeData$N*basal_area_ratio
            } 
          }
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

