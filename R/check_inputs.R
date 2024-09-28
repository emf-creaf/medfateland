#' Check spatial inputs
#' 
#' Functions to check and correct spatial inputs for simulations
#'
#' @param x An object of class \code{\link[sf]{sf}} to be checked.
#' @param default_values Vector of default values for locations with missing data.
#' @param missing_action Action to perform for missing values, either "no_action" (for checks), "filter" (filter missing data), "default" (impute default values)
#' @param verbose Logical flag to indicate extra console output.
#' @param progress A logical flag to print information about progress.
#'
#' @details
#' Function \code{check_topography()} checks that columns \code{"elevation"}, \code{"slope"} and \code{"aspect"} do not contain missing values.
#' 
#' Function \code{check_land_cover()} checks that column \code{"land_cover_type"} does not contain missing values.
#' 
#' Function \code{check_forests()} checks first that \code{\link[medfate]{forest}} objects are defined in "wildland" locations. Then, it looks for missing
#' data in tree or shrub attributes required for simulations. If \code{SpParams} is provided, the function also checks
#' whether species names are within the taxa represented in \code{SpParams}.
#' 
#' Function \code{check_soils()} checks first that "wildland" and "agriculture" locations have a defined soil object. Then it looks for missing data in required
#' soil physical parameters.
#' 
#' @return All functions return a modified \code{\link[sf]{sf}} object if \code{missing_action} is either \code{"filter"} or \code{"default"}. Otherwise,
#' they return an invisible tibble with logical columns indicating where missing information is. 
#'
#' @export
#'
#' @name check_inputs
#' @examples
#' data(example_ifn)
#' 
#' check_topography(example_ifn)
#' check_land_cover(example_ifn)
#' check_forests(example_ifn)
#' check_soils(example_ifn)
check_topography<-function(x, 
                           missing_action = "no_action",
                           default_values = c("elevation" = 0, "slope" = NA, "aspect" = NA),
                           verbose = TRUE) {
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  match.arg(missing_action, c("no_action", "filter", "default"))
  if(!("elevation" %in% names(x))) cli::cli_abort("Column 'elevation' must be defined.")
  if(!("slope" %in% names(x))) cli::cli_abort("Column 'slope' must be defined.")
  if(!("aspect" %in% names(x))) cli::cli_abort("Column 'aspect' must be defined.")
  mis_elevation <- is.na(x$elevation)
  mis_slope <- is.na(x$slope)
  mis_aspect <- is.na(x$aspect)
  mis_any <- mis_elevation | mis_slope | mis_aspect
  if(any(mis_elevation)) cli::cli_alert_warning(paste0("Found ", sum(mis_elevation), " locations with missing elevation."))
  if(any(mis_slope)) cli::cli_alert_warning(paste0("Found ", sum(mis_slope), " locations with missing slope."))
  if(any(mis_aspect)) cli::cli_alert_warning("Found ", paste0(sum(mis_aspect), " locations with missing aspect."))
  if(!any(mis_any)) {
    if(verbose) cli::cli_alert_success("No missing values in topography.")
  } 
  if(missing_action=="no_action") {
    out <- data.frame(missing_elevation = mis_elevation,
                      missing_slope = mis_slope,
                      missing_aspect = mis_aspect)
    return(invisible(tibble::as_tibble(out)))
  } else if(missing_action=="filter") {
    if(!any(mis_any)){
      if(verbose) cli::cli_alert_info(paste0("Filtering out ", sum(mis_any), " locations (",round(100*sum(mis_any)/nrow(x),1),"%) with missing topography."))
      x <- x[!mis_any,,drop =FALSE]
    }
  } else if(missing_action =="default") {
    if(any(mis_any)){
      if(verbose) cli::cli_alert_info(paste0("Filling ", sum(mis_any), " locations (",round(100*sum(mis_any)/nrow(x),1),"%) with default topography."))
      x$elevation[mis_elevation] <- default_values["elevation"]
      x$slope[mis_slope] <- default_values["slope"]
      x$aspect[mis_aspect] <- default_values["aspect"]
    }
  }
  return(x)
}

#' @export
#' @rdname check_inputs
check_land_cover<-function(x, 
                           missing_action = "no_action",
                           default_values = "wildland",
                           verbose = TRUE) {
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  match.arg(missing_action, c("no_action", "filter", "default"))
  if(!("land_cover_type" %in% names(x))) cli::cli_abort("Column 'land_cover_type' must be defined.")
  mis_land_cover <- is.na(x$land_cover_type)
  if(!any(mis_land_cover)) {
    if(verbose) cli::cli_alert_success("No missing values in land cover.")
  } else {
    if(verbose) cli::cli_alert_warning(paste0("Found ", sum(mis_land_cover), " locations with missing land cover."))
  }
  if(missing_action=="no_action") {
    out <- data.frame(missing_land_cover = mis_land_cover)
    return(invisible(tibble::as_tibble(out)))
  } else if(missing_action=="filter") {
    if(!any(mis_land_cover)) {
      if(verbose) cli::cli_alert_info(paste0("Filtering out ", sum(mis_land_cover), " locations (",round(100*sum(mis_land_cover)/nrow(x),1),"%) with missing land cover."))
      x <- x[!mis_land_cover,,drop =FALSE]
    }
  } else if(missing_action =="default") {
    if(!any(mis_land_cover)) {
      if(verbose) cli::cli_alert_info(paste0("Filling ", sum(mis_land_cover), " locations (",round(100*sum(mis_land_cover)/nrow(x),1),"%) with default land cover."))
      x$land_cover_type[mis_land_cover] <- default_values
    }
  }
  return(x)
}

#' @rdname check_inputs
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @export
check_forests <-function(x, SpParams = NULL,
                         missing_action = "no_action",
                         progress = FALSE, 
                         verbose = TRUE) {
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  match.arg(missing_action, c("no_action", "filter"))
  accepted_tree_names <- NULL
  accepted_shrub_names <- NULL
  if(!is.null(SpParams)) {
    if(!inherits(SpParams, "data.frame")) cli::cli_abort("'SpParams' should be a data frame with species parameters")
    if(!("Name" %in% names(SpParams))) cli::cli_abort("'SpParams' should contain a column 'Name'")
    accepted_tree_names <- SpParams$Name[SpParams$GrowthForm %in% c("Tree", "Tree/Shrub")]
    accepted_shrub_names <- SpParams$Name[SpParams$GrowthForm %in% c("Shrub", "Tree/Shrub")]
  }
  if(!("forest" %in% names(x))) cli::cli_abort("Column 'forest' must be defined.")
  npoints <- nrow(x)
  land_cover_type <- rep("wildland", nrow(x))
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  is_wildland <- land_cover_type %in% c("wildland")
  nwildland <- sum(is_wildland) 
  is_forest <- !unlist(lapply(x$forest, is.null))
  if(progress) {
    cli::cli_progress_step("Checking forest list")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  mis_forest <- rep(FALSE, npoints)
  wrong_class <- rep(FALSE, npoints)
  mis_tree_N <- rep(FALSE, npoints)
  mis_tree_DBH <- rep(FALSE, npoints)
  mis_tree_height <- rep(FALSE, npoints)
  mis_tree_species <- rep(FALSE, npoints)
  wrong_tree_species <- rep(FALSE, npoints)
  mis_shrub_cover <- rep(FALSE, npoints)
  mis_shrub_height <- rep(FALSE, npoints)
  mis_shrub_species <- rep(FALSE, npoints)
  wrong_shrub_species <- rep(FALSE, npoints)
  
  nr_tree <- 0
  nr_mis_tree_height <- 0
  nr_mis_tree_species <- 0
  nr_wrong_tree_species <- 0
  nr_mis_tree_N <- 0
  nr_mis_tree_DBH <- 0
  nr_shrub <- 0
  nr_mis_shrub_cover <- 0
  nr_mis_shrub_height <- 0
  nr_mis_shrub_species <- 0
  nr_wrong_shrub_species <- 0
  for(i in 1:npoints) {
    if(progress) cli::cli_progress_update()
    if(land_cover_type[i] == "wildland") {
      f <- x$forest[[i]]
      if(!is.null(f)) {
        if(inherits(f, "forest")) {
          if(nrow(f$treeData)>0) {
            nr_tree <- nr_tree + nrow(f$treeData)
            nr_mis_tree_N <- nr_mis_tree_N + sum(is.na(f$treeData$N))
            mis_tree_N[i] <- any(is.na(f$treeData$N))
            nr_mis_tree_DBH <- nr_mis_tree_DBH + sum(is.na(f$treeData$DBH))
            mis_tree_DBH[i] <- any(is.na(f$treeData$DBH))
            nr_mis_tree_height <- nr_mis_tree_height+ sum(is.na(f$treeData$Height))
            mis_tree_height[i] <- any(is.na(f$treeData$Height))
            nr_mis_tree_species <- nr_mis_tree_species + sum(is.na(f$treeData$Species))
            mis_tree_species[i] <- any(is.na(f$treeData$Species))
            if(!is.null(accepted_tree_names)) {
              wrong_spp  <- !(f$treeData$Species[!is.na(f$treeData$Species)] %in% accepted_tree_names)
              nr_wrong_tree_species <- nr_wrong_tree_species + sum(wrong_spp)
              wrong_tree_species[i] <- any(wrong_spp)
            }
          }
          if(nrow(f$shrubData)>0) {
            nr_shrub <- nr_shrub + nrow(f$shrubData)
            nr_mis_shrub_cover <- nr_mis_shrub_cover + sum(is.na(f$shrubData$Cover))
            mis_shrub_cover[i] <- any(is.na(f$shrubData$Cover))
            nr_mis_shrub_height <- nr_mis_shrub_height + sum(is.na(f$shrubData$Height))
            mis_shrub_height[i] <- any(is.na(f$shrubData$Height))
            nr_mis_shrub_species <- nr_mis_shrub_species + sum(is.na(f$shrubData$Species))
            mis_shrub_species[i] <- any(is.na(f$shrubData$Species))
            if(!is.null(accepted_shrub_names)) {
              wrong_spp <- !(f$shrubData$Species[!is.na(f$shrubData$Species)] %in% accepted_shrub_names)
              nr_wrong_shrub_species <- nr_wrong_shrub_species + sum(wrong_spp)
              wrong_shrub_species[i] <- any(wrong_spp)
            }
          }
        } else {
          wrong_class[i] <- TRUE
        }
      } else {
        mis_forest[i] <- TRUE
      }
    }
  }
  if(progress) {
    cli::cli_progress_done()
  }
  if(sum(mis_forest)>0) cli::cli_alert_warning(paste0("Missing 'forest' data in ", sum(mis_forest), " wildland locations (", round(100*sum(mis_forest)/nwildland ,1) ,"%)."))
  else if(verbose) cli::cli_alert_success("No wildland locations with NULL values in column 'forest'.")
  if(sum(wrong_class)>0) cli::cli_alert_warning(paste0("Wrong class in 'forest' column for ", sum(wrong_class), " wildland locations (", round(100*sum(wrong_class)/nwildland ,1) ,"%)."))
  else if(verbose) cli::cli_alert_success("All objects in column 'forest' have the right class.")
  if(sum(mis_tree_species)>0) cli::cli_alert_warning(paste0("Missing tree species detected for ", nr_mis_tree_species, " records (", round(100*nr_mis_tree_species/nr_tree,1) ,"%) in ", sum(mis_tree_species), " wildland locations (", round(100*sum(mis_tree_species)/nwildland ,1) ,"%)."))
  if(sum(wrong_tree_species)>0) cli::cli_alert_warning(paste0("Wrong tree species names detected for ", nr_wrong_tree_species, " records (", round(100*nr_wrong_tree_species/nr_tree,1) ,"%) in ", sum(wrong_tree_species), " wildland locations (", round(100*sum(wrong_tree_species)/nwildland ,1) ,"%)."))
  if(sum(mis_tree_N)>0) cli::cli_alert_warning(paste0("Missing tree density values detected for ", nr_mis_tree_N, " records (", round(100*nr_mis_tree_N/nr_tree,1) ,"%) in ", sum(mis_tree_N), " wildland locations (", round(100*sum(mis_tree_N)/nwildland ,1) ,"%)."))
  if(sum(mis_tree_height)>0) cli::cli_alert_warning(paste0("Missing tree height values detected for ", nr_mis_tree_height, " (", round(100*nr_mis_tree_height/nr_tree,1) ,"%) in ", sum(mis_tree_height), " wildland locations (", round(100*sum(mis_tree_height)/nwildland ,1) ,"%)."))
  if(sum(mis_tree_DBH)>0) cli::cli_alert_warning(paste0("Missing tree dbh values detected for ", nr_mis_tree_DBH, " records (", round(100*nr_mis_tree_DBH/nr_tree,1) ,"%) in ", sum(mis_tree_DBH), " wildland locations (", round(100*sum(mis_tree_DBH)/nwildland ,1) ,"%)."))
  if(sum(mis_shrub_species)>0) cli::cli_alert_warning(paste0("Missing shrub species detected for ", nr_mis_shrub_species, " records (", round(100*nr_mis_shrub_species/nr_shrub,1) ,"%) in ", sum(mis_shrub_species), " wildland locations (", round(100*sum(mis_shrub_species)/nwildland ,1) ,"%)."))
  if(sum(wrong_shrub_species)>0) cli::cli_alert_warning(paste0("Wrong shrub species names detected for ", nr_wrong_shrub_species, " records (", round(100*nr_wrong_shrub_species/nr_shrub,1) ,"%) in ", sum(wrong_shrub_species), " wildland locations (", round(100*sum(wrong_shrub_species)/nwildland ,1) ,"%)."))
  if(sum(mis_shrub_cover)>0) cli::cli_alert_warning(paste0("Missing shrub cover values detected for ", nr_mis_shrub_cover, " records (", round(100*nr_mis_shrub_cover/nr_shrub,1) ,"%) in ", sum(mis_shrub_cover), " wildland locations (", round(100*sum(mis_shrub_cover)/nwildland ,1) ,"%)."))
  if(sum(mis_shrub_height)>0) cli::cli_alert_warning(paste0("Missing shrub height values detected for ", nr_mis_shrub_height, " records (", round(100*nr_mis_shrub_height/nr_shrub,1) ,"%) in ", sum(mis_shrub_height), " wildland locations (", round(100*sum(mis_shrub_height)/nwildland ,1) ,"%)."))
  
  mis_tree_any  <- mis_tree_species | mis_tree_DBH | mis_tree_height | mis_tree_N
  mis_shrub_any  <- mis_shrub_species | mis_shrub_height | mis_shrub_cover
  error_any <- mis_tree_any | wrong_tree_species | mis_shrub_any | wrong_shrub_species
  if(sum(error_any)==0) if(verbose) cli::cli_alert_success("No missing/wrong values detected in key tree/shrub attributes of 'forest' objects.")
  
  if(missing_action=="no_action") {
    out <- data.frame(missing_forest = mis_forest,
                      wrong_forest_class = wrong_class,
                      tree_species = mis_tree_species,
                      wrong_tree_species = wrong_tree_species,
                      tree_dbh = mis_tree_DBH,
                      tree_height = mis_tree_height,
                      tree_density = mis_tree_N,
                      shrub_species = mis_shrub_species,
                      wrong_shrub_species = wrong_shrub_species,
                      shrub_cover = mis_shrub_cover,
                      shrub_height = mis_shrub_height)
    return(invisible(tibble::as_tibble(out)))
  } else if(missing_action=="filter") {
    if(any(error_any)) {
      if(verbose) cli::cli_alert_info(paste0("Filtering out problematic tree/shrub records in ", sum(error_any), " locations (",round(100*sum(error_any)/npoints,1),"%) with missing/wrong forest data."))
      if(progress) {
        cli::cli_progress_bar("Locations", total = nrow(x))
      }
      for(i in 1:npoints) {
        if(error_any[i]) {
          if(progress) cli::cli_progress_update()
          if(land_cover_type[i] == "wildland") {
            f <- x$forest[[i]]
            if(!is.null(f)) {
              if(wrong_class[i]) {
                x$forest[[i]] <- list(NULL)
              } else {
                if(mis_tree_any [i]) {
                  f$treeData <- f$treeData |>
                    dplyr::filter(is.na(.data$Species) & is.na(.data$DBH) & is.na(.data$Height) & is.na(.data$N))
                } else if(mis_shrub_any [i]) {
                  f$shrubData <- f$shrubData |>
                    dplyr::filter(is.na(.data$Species) & is.na(.data$Cover) & is.na(.data$Height))
                }
                if(wrong_tree_species[i]) {
                  f$treeData <- f$treeData |>
                    dplyr::filter(.data$Species %in% accepted_tree_names)
                }
                if(wrong_shrub_species[i]) {
                  f$shrubData <- f$shrubData |>
                    dplyr::filter(.data$Species %in% accepted_shrub_names)
                }
                x$forest[[i]] <- f
              }
            }
          }
        }
      }
      if(progress) cli::cli_progress_done()
    }
  } 
  return(x)
}


#' @rdname check_inputs
#' @param check_equal_layers Logical flag to test whether soils have the same number of layers.
#' @export
check_soils <-function(x, 
                       check_equal_layers = FALSE,
                       missing_action = "no_action",
                       default_values = c("clay" = 25, "sand" = 25, "bd" = 1.5, "rfc" = 25),
                       progress = FALSE,
                       verbose = TRUE) {
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  match.arg(missing_action, c("no_action", "default"))
  npoints <- nrow(x)
  if(!("soil" %in% names(x))) cli::cli_abort("Column 'soil' must be defined.")
  land_cover_type <- rep("wildland", npoints)
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  soil_cover <- land_cover_type %in% c("wildland", "agriculture")
  ntarget <- sum(soil_cover)
  is_soil <- !unlist(lapply(x$soil, is.null))
  if(sum(!(is_soil & soil_cover))>0) cli::cli_alert_info(paste0(sum(!(is_soil & soil_cover)), " null 'soil' elements out of ", ntarget," wildland/agriculture locations (",round(100*sum(!(is_soil & soil_cover))/ntarget,1),"%)."))
  if(sum(is_soil & soil_cover) < sum(soil_cover)) cli::cli_alert_warning(paste0("Soil needs to be defined for: ",sum(soil_cover) - sum(is_soil & soil_cover), " locations (",round(100*(sum(soil_cover) - sum(is_soil & soil_cover))/ntarget,1),"%)."))
  else if(verbose) cli::cli_alert_success("No wildland/agriculture locations with NULL values in column 'soil'.")
  
  if(progress) {
    cli::cli_progress_step("Checking for missing values in key soil parameters")
    cli::cli_progress_bar("Locations", total = nrow(x))
  }
  mis_widths <- 0
  mis_clay <- 0
  mis_sand <- 0
  mis_bd <- 0
  mis_rfc <- 0
  widths <- NA
  unequal_layers <- FALSE
  rfc_over_100 <- FALSE
  for(i in 1:npoints) {
    if(progress) cli::cli_progress_update()
    if(is_soil[i]) {
      s <- x$soil[[i]]
      if(any(is.na(s$widths))) {
        mis_widths <- mis_widths +1
      } else if(check_equal_layers) {
        if(!is.na(widths)) {
          widths <- s$widths
        } else {
          if(length(widths)!=length(s$widths)) unequal_layers <- TRUE
        }
      }
      
      if(any(is.na(s$clay))) {
        mis_clay <- mis_clay + 1
        if(missing_action=="default") s$clay[is.na(s$clay)] <- default_values["clay"]
      }
      if(any(is.na(s$sand))) {
        mis_sand <- mis_sand + 1
        if(missing_action=="default") s$sand[is.na(s$sand)] <- default_values["sand"]
      }
      if(any(is.na(s$bd))) {
        mis_bd <- mis_bd + 1
        if(missing_action=="default") s$bd[is.na(s$bd)] <- default_values["bd"]
      }
      if(any(is.na(s$rfc))) {
        mis_rfc <- mis_rfc + 1
        if(missing_action=="default") s$rfc[is.na(s$rfc)] <- default_values["rfc"]
      } else {
        if(any(s$rfc>=100)) rfc_over_100 <-TRUE
      }
      x$soil[[i]] <- s
    }
  }
  if(progress) {
    cli::cli_progress_done()
  }
  if(unequal_layers) cli::cli_alert_warning("Unequal number of layers were detected.")
  if(rfc_over_100) cli::cli_alert_warning("At least one soil with rfc values >= 100%.")
  if(missing_action=="no_action") {
    if(mis_widths>0) cli::cli_alert_warning(paste0("Missing 'widths' values detected for ", mis_clay, " locations (", round(100*mis_widths/ntarget,1),"%)."))
    if(mis_clay>0) cli::cli_alert_warning(paste0("Missing 'clay' values detected for ", mis_clay, " locations (", round(100*mis_clay/ntarget,1),"%)."))
    if(mis_sand>0) cli::cli_alert_warning(paste0("Missing 'sand' values detected for ", mis_sand, " locations (", round(100*mis_sand/ntarget,1),"%)."))
    if(mis_bd>0) cli::cli_alert_warning(paste0("Missing 'bd' values detected for ", mis_bd, " locations (", round(100*mis_bd/ntarget,1),"%)."))
    if(mis_rfc>0) cli::cli_alert_warning(paste0("Missing 'rfc' values detected for ", mis_rfc, " locations (", round(100*mis_rfc/ntarget,1),"%)."))
  } else {
    if(mis_widths>0) cli::cli_alert_warning(paste0("Missing 'widths' values detected for ", mis_widths, " locations (", round(100*mis_widths/ntarget,1),"%) and cannot be imputed!"))
    if(mis_clay>0) cli::cli_alert_info(paste0("Default 'clay' values assigned for ", mis_clay, " locations (", round(100*mis_clay/ntarget,1),"%)."))
    if(mis_sand>0) cli::cli_alert_info(paste0("Default 'sand' values assigned for ", mis_sand, " locations (", round(100*mis_sand/ntarget,1),"%)."))
    if(mis_bd>0) cli::cli_alert_info(paste0("Default 'bd' values assigned for ", mis_bd, " locations (", round(100*mis_bd/ntarget,1),"%)."))
    if(mis_rfc>0) cli::cli_alert_info(paste0("Default 'rfc' values assigned for ", mis_rfc, " locations (", round(100*mis_rfc/ntarget,1),"%)."))
  }
  if(mis_clay==0 && mis_bd==0 && mis_rfc ==0 && mis_sand==0) if(verbose) cli::cli_alert_success("No missing values detected in key soil attributes.")
  if(missing_action=="no_action") {
    out <- data.frame(widths = mis_widths,
                      clay = mis_clay,
                      sand = mis_sand,
                      bd = mis_bd,
                      rfc = mis_rfc)
    return(invisible(tibble::as_tibble(out)))
  } else {
    return(x)
  }
}
