#' Landscape soil parametrization
#'
#' Function \code{add_soilgrids} fills column 'soil' with physical soil characteristics drawn from SoilGrids 2.0 (Hengl et al. 2017; Poggio et al. 2021). 
#' Function \code{modify_soils} modifies soil definition according to soil depth and depth to bedrock information.
#' Function \code{check_soils} verifies that soil data does not contain missing values for key variables and, if so, assigns default values. 
#'
#' @param x An object of class \code{\link[sf]{sf}} with a valid CRS definition. If it contains a column called 'land_cover_type', soils will be retrieved for "agriculture" and "wildland" 
#'          cover types only. Otherwise, soils are retrieved for all locations. For functions \code{modify_soils} or \code{check_soils}, \code{x} should already contain a column named "soil".
#' @param soilgrids_path Path to SoilGrids rasters (see details). If missing, the SoilGrids REST API (https://rest.isric.org) will be queried.
#' @param widths A numeric vector indicating the desired layer widths, in \emph{mm}. If \code{NULL} the default soil grids layer definition is returned.
#' @param replace_existing A logical flag to force the replacement of existing soil data, when already present
#' @param progress A logical flag to include a progress bar while processing the output of the query to the SoilGrids REST API.
#'
#' @details 
#' 
#' If \code{soilgrids_path = NULL} the function connects with the SoilGrids REST API (https://rest.isric.org)
#' to retrieve the soil physical and chemical characteristics for a site (Hengl \emph{et al}. 2007; Poggio et al. 2021), selected
#' by its coordinates. Also, in case the depths are not the default ones in the SoilGrids API, the function uses
#' averages the values of soil grid layers depending on the overlap between soil layer definitions. Unfortunately,
#' SoilGrids REST API queries are limited to a few points.
#' 
#' If \code{soilgrids_path != NULL} the function will read SoilGrid rasters from the file disk. Folders need to be defined
#' for each variable ("sand", "clay", "soc", "bdod", "cfvo" and "nitrogen"). File paths from \code{soilgrids_path} should be named:
#' 
#' \emph{var}/\emph{var}_\emph{layer}_mean.tif
#' 
#' where \emph{var} is one of the above and \emph{layer} is "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm" or "100-200cm"
#' 
#' SoilGrids does not provide soil depth estimates. Function \code{modify_soils} is designed to adjust soil depths according to available information.
#' When \code{soil_depth_map} is provided, the function adjusts rock fragment content of layers below soil depth with the value of \code{regolith_rfc}. 
#' When \code{depth_to_bedrock_map} is provided, the function truncates the total depth of the soil definition to the depth to bedrock.
#' If regional maps of soil depth are not available, users are recommended to resort on Shangguan et al (2017).
#' 
#' @return A modified object of class \code{\link[sf]{sf}} with column 'soil'.
#'
#' @author \enc{Víctor}{Victor} Granda, EMF-CREAF
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @encoding UTF-8
#' @export
#' @name soil_parametrization
#' @references
#' Hengl T, Mendes de Jesus J, Heuvelink GBM, Ruiperez Gonzalez M, Kilibarda M, \enc{Blagotić}{Blagotic} A, et al. (2017) SoilGrids250m: Global gridded soil information based on machine learning. PLoS ONE 12(2): e0169748. doi:10.1371/journal.pone.0169748.
#'
#' Poggio L, de Sousa LM, Batjes NH, Heuvelink GBM, Kempen B, Ribeiro E, Rossiter D (2021). SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. SOIL 7, 217-240. doi: 10.5194/soil-7-217-2021
#' 
#' Shangguan W, Hengl T, Mendes de Jesus J, Yuan H, Dai J (2017). Mapping the global depth to bedrock for land surface modeling. Journal of Advances in Modeling Earth Systems 9: 65-88. doi: 10.1002/2016MS000686
#' 
#' @seealso [add_topography()], [impute_forests()], \code{\link[medfate]{soil}}, \code{\link[medfate]{defaultSoilParams}}
#'
#' @examples
#'  \donttest{
#'    library(sf)
#'    x <- st_sf(geometry = st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326))
#'    x_soil <- add_soilgrids(x, widths = c(300, 700, 1000))
#'    x_soil
#'    # See more complete examples in package vignettes 'Preparing inputs'
#'  }
#'
add_soilgrids <- function(x, soilgrids_path = NULL, 
                          widths = NULL, replace_existing = TRUE, 
                          progress = TRUE) {
  if(!inherits(x, "sf"))  cli::cli_abort("Object 'x' has to be of class 'sf'")
  x_lonlat <- sf::st_transform(sf::st_geometry(x), 4326)
  coords <- sf::st_coordinates(x_lonlat)
  npoints <- nrow(coords)
  land_cover_type <- rep("wildland", npoints)
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  nsoil <- sum(land_cover_type %in% c("wildland", "agriculture"))
  
  if(!("soil" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining new column 'soil'")
    x$soil <- vector("list", npoints)
  }
  if(is.null(soilgrids_path)) {
    if(progress) {
      cli::cli_progress_step(paste0("Querying ", nsoil," points to rest.isric.org:\n"))
      cli::cli_progress_bar(name = "Points", total = npoints)
    }
    url.base <- "https://rest.isric.org/soilgrids/v2.0/properties/query?"
    
    props_str <- "property=bdod&property=cfvo&property=clay&property=ocd&property=ocs&property=sand&property=silt&property=soc&property=nitrogen"
    depths_str <- "depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm"
    for(i in 1:npoints) {
      if(progress) cli::cli_progress_update()
      if(land_cover_type[i] %in% c("wildland", "agriculture")) {
        tryCatch( {
          resSG <- data.frame(matrix(nrow = 6, ncol = 6))
          names(resSG) <- c("widths", "clay", "sand", "om", "bd", "rfc")
          resSG$widths <- c(50,100,150,300,400,1000)
          coord_str <- paste0("lon=",coords[i,1],"&lat=", coords[i,2])
          dest <- paste(coord_str, props_str, depths_str,"value=mean",sep="&")
          url1 <- paste0(url.base, dest)
          path1 <- httr::GET(url1, httr::add_headers("accept"= "application/json"))
          ans.text <- httr::content(path1, as = "text", encoding = "utf-8")
          ans <- jsonlite::fromJSON(ans.text)
          propNames <- ans$properties$layers$name
          d_factors <- ans$properties$layers$unit_measure$d_factor
          for(j in 1:length(propNames)) {
            if(propNames[j]=="clay") {
              resSG$clay = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="sand") {
              resSG$sand = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="bdod") {
              resSG$bd = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="soc") {
              resSG$om = ans$properties$layers$depths[[j]]$values$mean/(d_factors[j]*10)
            } else if(propNames[j]=="nitrogen") {
              resSG$nitrogen = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="cfvo") {
              resSG$rfc = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            }
          }
          if(is.null(x$soil[[i]]) || replace_existing) {
            if(!is.null(widths)) {
              x$soil[[i]] = medfate::soil_redefineLayers(resSG, widths)
            } else {
              x$soil[[i]] = resSG
            }
          }
        }, error  = function(cond) {
          cli::cli_alert_warning(paste("Problems retrieving point",i,": ", cond,"\n"))
        })
      }
    }
    if(progress) cli::cli_progress_done()
  } else {
    if(progress) {
      cli::cli_progress_step(paste0("Extracting ", nsoil," points from SoilGrids raster layers."))
    }
    vars <- c("sand", "clay", "soc", "nitrogen", "bdod", "cfvo")
    layers <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
    m_var_list <- vector("list", length(vars)) 
    names(m_var_list) <- vars
    for(v in 1:length(vars)) {
      var <- vars[v]
      m_var <- matrix(NA, nrow = nrow(x), ncol = length(layers))
      for(l in 1:length(layers)) {
        layer = layers[l]
        tif_file <- paste0(soilgrids_path, var,"/", var, "_",layer,"_mean.tif")
        r <- terra::rast(tif_file)
        x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(r)))
        m_var[,l] <- terra::extract(r, x_vect)[,2]
      }
      m_var_list[[v]] <- m_var
    }
    for(i in 1:npoints) {
      if(land_cover_type[i] %in% c("wildland", "agriculture")) {
        resSG = data.frame(matrix(nrow = 6, ncol = 6))
        names(resSG) = c("widths", "clay", "sand", "om", "bd", "rfc")
        resSG$widths = c(50,100,150,300,400,1000)
        for(var in vars) {
          if(var=="clay") {
            resSG$clay <- m_var_list[["clay"]][i,]/10
          } else if(var=="sand") {
            resSG$sand <- m_var_list[["sand"]][i,]/10
          } else if(var=="soc") {
            resSG$om <- m_var_list[["soc"]][i,]/100
          } else if(var=="bdod") {
            resSG$bd <- m_var_list[["bdod"]][i,]/100
          } else if(var=="cfvo") {
            resSG$rfc <- m_var_list[["cfvo"]][i,]/10
          } else if(var=="nitrogen") {
            resSG$nitrogen <- m_var_list[["nitrogen"]][i,]/100
          } 
        }
        if(is.null(x$soil[[i]]) || replace_existing) {
          if(!is.null(widths)) {
            x$soil[[i]] = medfate::soil_redefineLayers(resSG, widths)
          } else {
            x$soil[[i]] = resSG
          }
        }
      }
    }
  }
  return(sf::st_as_sf(tibble::as_tibble(x)))
}


.modify_soil_definition <-function(soildf, soil_depth = NA, depth_to_bedrock = NA, 
                                   regolith_rfc = 97.5, full_rock_filling = TRUE) {
  nl = nrow(soildf)
  oridepths = c(0, cumsum(soildf$widths[1:(nl-1)]))
  findepths = cumsum(soildf$widths[1:nl])
  orimaxdepth = sum(soildf$widths)
  # print(cbind(soildf$widths, oridepths, findepths))
  # Modify total soil depth according to bedrock
  if(!is.na(depth_to_bedrock)) {
    if(depth_to_bedrock < orimaxdepth) {
      to_remove <- (oridepths >= depth_to_bedrock)
      soildf <- soildf[!to_remove,,drop = FALSE]
      nl <- nrow(soildf)
      oridepths <- oridepths[!to_remove]
      findepths <- findepths[!to_remove]
      findepths[nl] <- depth_to_bedrock
      soildf$widths[nl] <- findepths[nl] - oridepths[nl]
    }
  }
  # Modify soil depth
  if(!is.na(soil_depth)) {
    if(full_rock_filling) {
      for(l in 1:nl) {
        mid_point <- oridepths[l] + soildf$widths[l]/2
        rel_dist_to_soil_depth <- min(1, mid_point/soil_depth)
        soildf$rfc[l]<- max(soildf$rfc[l], regolith_rfc*rel_dist_to_soil_depth)
      }
    } else {
      soildf$rfc[oridepths > soil_depth] <- regolith_rfc
      i_int <- which((oridepths < soil_depth) & (findepths > soil_depth))
      if(length(i_int)>0) {
        frac_above = (soil_depth - oridepths[i_int])/(findepths[i_int]-oridepths[i_int])
        soildf$rfc[i_int]<- soildf$rfc[i_int]*frac_above + regolith_rfc*(1-frac_above)
      }
    }
  }
  return(soildf)
}

#' @param soil_depth_map An object of class \code{\link[terra]{SpatRaster}} or \code{\link[terra]{SpatVector}} with the soil depth (in \emph{mm}) values.
#' @param depth_to_bedrock_map An object of class \code{\link[terra]{SpatRaster}} or \code{\link[terra]{SpatVector}} with depth to bedrock (in \emph{mm}) values.
#' @param regolith_rfc Rock fragment content, in percent volume, between soil depth and 200cm depth (or lower depths, if modified via \code{widths}).
#' @param full_rock_filling Logical flag to modify rock fragment content in all soil layers with according to distance to soil depth.
#'
#' @rdname soil_parametrization
#' @export
modify_soils <- function(x, soil_depth_map = NULL,
                         depth_to_bedrock_map = NULL, 
                         regolith_rfc = 97.5, full_rock_filling = TRUE,
                         progress = TRUE) {
  if(progress) cli::cli_progress_step("Checking inputs")
  if(!inherits(x, "sf"))  cli::cli_abort("Object 'x' has to be of class 'sf'")
  if(!("soil" %in% names(x))) cli_abort("Object 'x' should have a column called 'soil'")
  if(is.null(soil_depth_map) && !is.null(depth_to_bedrock_map)) cli_abort("Either 'soil_depth_map' or 'depth_to_bedrock_map' should be provided")
  if(!is.null(soil_depth_map)) {
    if(!inherits(soil_depth_map, "SpatRaster") && !inherits(soil_depth_map, "SpatVector")) cli_abort("'soil_depth_map' should be of class 'SpatRaster' or 'SpatVector'")
  }
  if(!is.null(depth_to_bedrock_map)) {
    if(!inherits(depth_to_bedrock_map, "SpatRaster") && !inherits(depth_to_bedrock_map, "SpatVector")) cli_abort("'depth_to_bedrock_map' should be of class 'SpatRaster' or 'SpatVector'")
  }
  npoints <- nrow(x)
  is_soil <- !unlist(lapply(x$soil, is.null))

  x_soil_depth <- rep(NA, npoints)
  if(!is.null(soil_depth_map)) {
    if(progress) cli::cli_progress_step("Extracting soil depth")
    x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(soil_depth_map)))
    x_soil_depth<-terra::extract(soil_depth_map, x_vect)[,2, drop = TRUE]
  }
  x_depth_to_bedrock <- rep(NA, npoints)
  if(!is.null(depth_to_bedrock_map)) {
    if(progress) cli::cli_progress_step("Extracting depth to bedrock")
    x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(depth_to_bedrock_map)))
    x_depth_to_bedrock<-terra::extract(depth_to_bedrock_map, x_vect)[,2, drop = TRUE]
  }
  if(!is.null(soil_depth_map) || !is.null(depth_to_bedrock_map)) {
    if(progress) {
      cli::cli_progress_step("Modifying soil depths")
      cli::cli_progress_bar("Locations", total = nrow(x))
    }
    for(i in 1:npoints) {
      if(progress) cli::cli_progress_update()
      if(is_soil[i]){
        if(!is.data.frame(x$soil[[i]])) cli_abort("Elements in 'soil' should be data frames of soil physical characteristics")
        x$soil[[i]] <- .modify_soil_definition(x$soil[[i]], 
                                               x_soil_depth[i], 
                                               x_depth_to_bedrock[i], 
                                               regolith_rfc,
                                               full_rock_filling)
      }
    }
    if(progress) {
      cli::cli_progress_done()
    }
  }
  return(sf::st_as_sf(tibble::as_tibble(x)))
}

#' @rdname soil_parametrization
#' @param fill_missing Logical flag to fill missing values in key parameters with defaults.
#' @param check_equal_layers Logical flag to test whether soils have the same number of layers.
#' @param default_values Vector of default values for locations with missing data.
#' @export
check_soils <-function(x, 
                       check_equal_layers = FALSE,
                       fill_missing = FALSE,
                       default_values = c("clay" = 25, "sand" = 25, "bd" = 1.5, "rfc" = 25),
                       progress = FALSE) {
  if(!inherits(x, "sf")) cli::cli_abort("'x' should be of class 'sf' ")
  npoints <- nrow(x)
  if(!("soil" %in% names(x))) cli::cli_abort("Column 'soil' must be defined.")
  land_cover_type <- rep("wildland", npoints)
  if("land_cover_type" %in% names(x)) land_cover_type <- x$land_cover_type 
  soil_cover <- land_cover_type %in% c("wildland", "agriculture")
  ntarget <- sum(soil_cover)
  is_soil <- !unlist(lapply(x$soil, is.null))
  cli::cli_alert_info(paste0(sum(is_soil & soil_cover), " non-null 'soil' elements out of ", ntarget," wildland/agriculture locations (",round(100*sum(is_soil & soil_cover)/ntarget,1),"%)."))
  if(sum(is_soil & soil_cover) < sum(soil_cover)) cli::cli_alert_warning(paste0("Soil needs to be defined for: ",sum(soil_cover) - sum(is_soil & soil_cover), " locations (",round(100*(sum(soil_cover) - sum(is_soil & soil_cover))/ntarget,1),"%)."))
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
        if(fill_missing) s$clay[is.na(s$clay)] <- default_values["clay"]
      }
      if(any(is.na(s$sand))) {
        mis_sand <- mis_sand + 1
        if(fill_missing) s$sand[is.na(s$sand)] <- default_values["sand"]
      }
      if(any(is.na(s$bd))) {
        mis_bd <- mis_bd + 1
        if(fill_missing) s$bd[is.na(s$bd)] <- default_values["bd"]
      }
      if(any(is.na(s$rfc))) {
        mis_rfc <- mis_rfc + 1
        if(fill_missing) s$rfc[is.na(s$rfc)] <- default_values["rfc"]
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
  if(!fill_missing) {
    if(mis_widths>0) cli::cli_alert_warning(paste0("Missing 'widths' values detected for ", mis_clay, " locations (", round(100*mis_widths/ntarget,1),"%)."))
    if(mis_clay>0) cli::cli_alert_warning(paste0("Missing 'clay' values detected for ", mis_clay, " locations (", round(100*mis_clay/ntarget,1),"%)."))
    if(mis_sand>0) cli::cli_alert_warning(paste0("Missing 'sand' values detected for ", mis_sand, " locations (", round(100*mis_sand/ntarget,1),"%)."))
    if(mis_bd>0) cli::cli_alert_warning(paste0("Missing 'bd' values detected for ", mis_bd, " locations (", round(100*mis_bd/ntarget,1),"%)."))
    if(mis_rfc>0) cli::cli_alert_warning(paste0("Missing 'rfc' values detected for ", mis_rfc, " locations (", round(100*mis_rfc/ntarget,1),"%)."))
  } else {
    if(mis_widths>0) cli::cli_alert_warning(paste0("Missing 'widths' values detected for ", mis_clay, " locations (", round(100*mis_widths/ntarget,1),"%) and cannot be imputed!"))
    if(mis_clay>0) cli::cli_alert_info(paste0("Default 'clay' values assigned for ", mis_clay, " locations (", round(100*mis_clay/ntarget,1),"%)."))
    if(mis_sand>0) cli::cli_alert_info(paste0("Default 'sand' values assigned for ", mis_sand, " locations (", round(100*mis_sand/ntarget,1),"%)."))
    if(mis_bd>0) cli::cli_alert_info(paste0("Default 'bd' values assigned for ", mis_bd, " locations (", round(100*mis_bd/ntarget,1),"%)."))
    if(mis_rfc>0) cli::cli_alert_info(paste0("Default 'rfc' values assigned for ", mis_rfc, " locations (", round(100*mis_rfc/ntarget,1),"%)."))
  }
  if(mis_clay==0 && mis_bd==0 && mis_rfc ==0 && mis_sand==0) cli::cli_alert_info("No missing values detected in key soil attributes.")
  if(fill_missing) return(x)
}

