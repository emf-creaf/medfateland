.getAllowedTopographyVars <-function(obj) {
  vars <- c( "Elevation (m)" = "elevation", 
     "Slope (degrees)" = "slope", 
     "Aspect (degrees)" = "aspect", 
     "Land cover type" = "land_cover_type")
  return(vars[vars %in% names(obj)])
}
.getLandscapeTopographyVar<-function(obj, variable) {
  if(!(variable %in% names(obj)))  cli::cli_abort(paste0("Object does not have a '", variable, "' column."))
  varplot = obj[[variable]]
  return(varplot)
}

.getAllowedSoilVars <-function(obj) {
  if(!("soil" %in% names(obj))) return(character(0))
  return(c("Total water extractable volume (mm)" = "soil_vol_extract",
           "Total water volume at saturation (mm)" = "soil_vol_sat",
           "Total water volume at field capacity (mm)" = "soil_vol_fc",
           "Total water volume at wilting point (mm)" = "soil_vol_wp",
           "Current total water volume (mm)" = "soil_vol_curr",
           "Soil relative water content (%)" = "soil_rwc_curr",
           "Soil relative extractable water (%)" = "soil_rew_curr",
           "Soil moisture content (% vol.)" = "soil_theta_curr",
           "Soil water potential (MPa)" = "soil_psi_curr"))
}
.getLandscapeSoilVar<-function(obj, variable) {
  if(!("soil" %in% names(obj))) cli::cli_abort("Object does not have a 'soil' column.")
  n = length(obj$soil)
  varplot = rep(NA, n)
  for(i in 1:n) {
    s = obj$soil[[i]]
    if(inherits(s, "data.frame")) s <- medfate::soil(s)
    if(inherits(s,"soil")) {
      if(variable=="soil_vol_extract") varplot[i] = sum(soil_waterExtractable(s), na.rm=TRUE)
      else if(variable=="soil_vol_sat") varplot[i] = sum(soil_waterSAT(s), na.rm=TRUE)
      else if(variable=="soil_vol_fc") varplot[i] = sum(soil_waterFC(s), na.rm=TRUE)
      else if(variable=="soil_vol_wp") varplot[i] = sum(soil_waterWP(s), na.rm=TRUE)
      else if(variable=="soil_vol_curr") varplot[i] = sum(soil_water(s), na.rm=TRUE)
      else if(variable=="soil_rwc_curr") varplot[i] = 100*sum(soil_water(s), na.rm=TRUE)/sum(soil_waterFC(s), na.rm=TRUE)
      else if(variable=="soil_theta_curr") varplot[i] = 100*sum(soil_theta(s)*soil_waterSAT(s), na.rm=TRUE)/sum(soil_waterSAT(s), na.rm =TRUE)
      else if(variable=="soil_rew_curr") varplot[i] = 100*sum(soil_water(s) - soil_waterPsi(s, psi = -5.0), na.rm=TRUE)/sum(soil_waterExtractable(s, minPsi = -5.0), na.rm =TRUE)
      else if(variable=="soil_psi_curr") varplot[i] = sum(soil_psi(s)*soil_waterSAT(s), na.rm=TRUE)/sum(soil_waterSAT(s), na.rm =TRUE)
    }
  }
  return(varplot)
}

.getAllowedWatershedVars <-function(obj){
  vars <- character(0)
  if("depth_to_bedrock" %in% names(obj)) vars <- c(vars, "Depth to bedrock (m)" = "depth_to_bedrock") 
  if("bedrock_porosity" %in% names(obj)) vars <- c(vars, "Bedrock porosity" = "bedrock_porosity") 
  if("bedrock_conductivity" %in% names(obj)) vars <- c(vars, "Bedrock conductivity (m/day)" = "bedrock_conductivity") 
  if("snowpack" %in% names(obj)) vars <- c(vars, "Snowpack water equivalent (mm)" = "snowpack") 
  if("aquifer" %in% names(obj)) {
    vars <- c(vars, "Aquifer volume (mm)" = "aquifer") 
    if("bedrock_porosity" %in% names(obj)) {
      vars <- c(vars, "Depth to aquifer (m)" = "depth_to_aquifer") 
      if("elevation" %in% names(obj)) {
        vars <- c(vars, "Aquifer elevation (m)" = "aquifer_elevation") 
      }
    }
  }
  return(vars)
}
.getLandscapeWatershedVar<-function(obj, variable) {
  if(variable =="depth_to_bedrock") {
    varplot = obj$depth_to_bedrock/1000.0  # in m
  } else if(variable =="bedrock_porosity") {
    varplot = obj$bedrock_porosity
  } else if(variable =="bedrock_conductivity") {
    varplot = obj$bedrock_conductivity
  } else if(variable =="aquifer") {
    varplot = obj$aquifer
  } else if(variable =="aquifer_elevation") {
    DTB = obj$depth_to_bedrock
    aquifer = obj$aquifer
    RockPorosity = obj$bedrock_porosity
    elevation = obj$elevation
    varplot = elevation - (DTB/1000.0) + (aquifer/RockPorosity)/1000.0 # in m
    varplot[RockPorosity==0.0] = elevation[RockPorosity==0.0]
  } else if(variable=="snowpack") {
    varplot = obj$snowpack
  } else if(variable =="depth_to_aquifer") {
    DTB = obj$depth_to_bedrock
    aquifer = obj$aquifer
    RockPorosity = obj$bedrock_porosity
    varplot = (DTB/1000.0) - (aquifer/RockPorosity)/1000.0
    varplot[RockPorosity==0.0] = DTB[RockPorosity==0.0]/1000
  }
  return(varplot)
}

.getAllowedForestStandVars <-function(obj, SpParams = NULL) {
  if(!("forest" %in% names(obj))) return(character(0))
  vars <- c("Basal area (m2/ha)"="basal_area",
            "Tree density (ind/ha)"="tree_density",
            "Mean tree height (cm)" = "mean_tree_height",
            "Dominant tree height (cm)" = "dominant_tree_height",
            "Dominant tree diameter (cm)" = "dominant_tree_diameter",
            "Quadratic mean tree diameter (cm)" = "quadratic_mean_tree_diameter",
            "Hart-Becking index" = "hart_becking_index")
  if(!is.null(SpParams)) {
    vars <- c(vars,
              "Leaf area index (m2/m2)" = "leaf_area_index", 
              "Foliar biomass (kg/m2)" = "foliar_biomass", 
              "Fine live fuel (kg/m2)" = "fuel_loading",
              "Shrub volume (m3/m2)" = "shrub_volume")
  }
  return(vars)
}
.getLandscapeForestStandVar<-function(obj, variable, SpParams = NULL) {
  if(!("forest" %in% names(obj)))  cli::cli_abort("Object does not have a 'forest' column.")
  n = length(obj$forest)
  varplot = rep(NA, n)
  for(i in 1:n) {
    f = obj$forest[[i]]
    if(inherits(f,"forest")) {
      if(variable=="basal_area") varplot[i] = stand_basalArea(f)
      else if(variable=="tree_density") varplot[i] = stand_treeDensity(f)
      else if(variable=="mean_tree_height") varplot[i] = stand_meanTreeHeight(f)
      else if(variable=="dominant_tree_height") varplot[i] = stand_dominantTreeHeight(f)
      else if(variable=="dominant_tree_diameter") varplot[i] = stand_dominantTreeDiameter(f)
      else if(variable=="quadratic_mean_tree_diameter") varplot[i] = stand_quadraticMeanTreeDiameter(f)
      else if(variable=="hart_becking_index") varplot[i] = stand_hartBeckingIndex(f)
      else if(variable=="leaf_area_index") varplot[i] = stand_LAI(f, SpParams)
      else if(variable=="foliar_biomass") varplot[i] = stand_foliarBiomass(f, SpParams)
      else if(variable=="fuel_loading") varplot[i] = stand_fuelLoading(f, SpParams)
      else if(variable=="shrub_volume") varplot[i] = stand_shrubVolume(f, SpParams)
    }
  }
  return(varplot)
}

.getAllowedVars <-function(y, SpParams = NULL) {
  vars = character(0)
  vars = c(vars, 
           .getAllowedTopographyVars(y),
           .getAllowedSoilVars(y),
           .getAllowedForestStandVars(y, SpParams),
           .getAllowedWatershedVars(y))
  return(vars)
}
.getLandscapeVar<-function(obj, variable, SpParams = NULL, ...) {
  variable = match.arg(variable, .getAllowedVars(obj, SpParams))
  if(variable %in% .getAllowedTopographyVars(obj)) return(.getLandscapeTopographyVar(obj, variable))
  else if(variable %in% .getAllowedSoilVars(obj)) return(.getLandscapeSoilVar(obj, variable))
  else if(variable %in% .getAllowedWatershedVars(obj)) return(.getLandscapeWatershedVar(obj, variable))
  else if(variable %in% .getAllowedForestStandVars(obj, SpParams)) return(.getLandscapeForestStandVar(obj, variable, SpParams))
}
.getDefaultScale<-function(variable, fill = FALSE, ...) {
  if(variable =="elevation") {
    if(fill) {
      scale <- scale_fill_distiller("m", palette="Spectral", ..., na.value = NA)
    } else {
      scale <- scale_color_distiller("m", palette="Spectral", ..., na.value = NA)
    }
  }
  else if(variable =="slope") {
    if(fill) {
      scale <- scale_fill_distiller("degrees", palette="Spectral", limits = c(0,90),..., na.value = NA)
    } else {
      scale <- scale_color_distiller("degrees", palette="Spectral", limits = c(0,90),..., na.value = NA)    
    }
  }
  else if(variable =="aspect") {
    if(fill) {
      scale <- scale_fill_distiller("degrees", 
                                    palette = "BrBG",
                                    limits = c(0,360),..., na.value = NA)
    } else {
      scale <- scale_color_distiller("degrees", 
                                     palette = "BrBG",
                                     limits = c(0,360),..., na.value = NA)    
    }
  } 
  else if(variable =="land_cover_type") {
    if(fill) {
      scale <- scale_fill_brewer("", palette = "Set1", ..., na.value = NA)
    } else {
      scale <- scale_color_brewer("",palette = "Set1", ..., na.value = NA)
    }
  }
  else if(variable %in% c("soil_vol_extract", "soil_vol_sat", "soil_vol_fc", "soil_vol_wp", "soil_vol_curr")) {
    if(fill) {
      scale <- scale_fill_distiller("mm", palette = "Blues",direction = 1,  ..., na.value = NA)
    } else {
      scale <- scale_color_distiller("mm", palette = "Blues", direction = 1, ..., na.value = NA)
    }
  }
  else if(variable %in% c("soil_rwc_curr", "soil_rew_curr")) {
    if(fill) {
      scale <- scale_fill_distiller("%", palette = "Blues", direction = 1, limits = c(0,150),..., na.value = NA)
    } else {
      scale <- scale_color_distiller("%", palette = "Blues",direction = 1, limits = c(0,150), ..., na.value = NA)
    }
  }
  else if(variable %in% c("soil_theta_curr")) {
    if(fill) {
      scale <- scale_fill_distiller("%", palette = "Blues",  direction = 1, limits = c(0,60), ..., na.value = NA)
    } else {
      scale <- scale_color_distiller("%", palette = "Blues", direction = 1, limits = c(0,60),..., na.value = NA)
    }
  }
  else if(variable =="soil_psi_curr") {
    if(fill) {
      scale <- scale_fill_continuous("MPa", type = "viridis", limits = c(-10,0),..., na.value = NA)
    } else {
      scale <- scale_color_continuous("MPa", type = "viridis", limits = c(-10,0),..., na.value = NA)
    }
  }
  else if(variable =="depth_to_bedrock") {
    if(fill) {
      scale <- scale_fill_continuous("m", type = "viridis", ..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m", type = "viridis", ..., na.value = NA)
    }
  }
  else if(variable =="bedrock_porosity") {
    if(fill) {
      scale <- scale_fill_continuous("", limits = c(0,0.2),..., na.value = NA)
    } else {
      scale <- scale_color_continuous("",  limits = c(0,0.2),..., na.value = NA)
    }
  }
  else if(variable =="bedrock_conductivity") {
    if(fill) {
      scale <- scale_fill_continuous("m/day",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m/day",..., na.value = NA)
    }
  }
  else if(variable %in% c("aquifer_elevation", "depth_to_aquifer")) {
    if(fill) {
      scale <- scale_fill_continuous("m",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m",..., na.value = NA)
    }
  }
  else if(variable %in% c("aquifer", "snowpack")) {
    if(fill) {
      scale <- scale_fill_continuous("mm",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("mm",..., na.value = NA)
    }
  }
  else if(variable =="basal_area") {
    if(fill) {
      scale <- scale_fill_continuous("m2/ha",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m2/ha",..., na.value = NA)
    }
  }
  else if(variable =="tree_density") {
    if(fill) {
      scale <- scale_fill_continuous("ind/ha",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("ind/ha",..., na.value = NA)
    }
  }
  else if(variable %in% c("mean_tree_height", "dominant_tree_height", "dominant_tree_diameter", "quadratic_mean_tree_diameter")) {
    if(fill) {
      scale <- scale_fill_continuous("cm",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("cm",..., na.value = NA)
    }
  }
  else if(variable =="hart_becking_index") {
    if(fill) {
      scale <- scale_fill_continuous("HB",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("HB",..., na.value = NA)
    }
  }
  else if(variable =="leaf_area_index") {
    if(fill) {
      scale <- scale_fill_continuous("m2/m2",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m2/m2",..., na.value = NA)
    }
  }
  else if(variable =="foliar_biomass") {
    if(fill) {
      scale <- scale_fill_continuous("kg/m2",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("kg/m2",..., na.value = NA)
    }
  }
  else if(variable =="fuel_loading") {
    if(fill) {
      scale <- scale_fill_continuous("kg/m2",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("kg/m2",..., na.value = NA)
    }
  }
  else if(variable =="shrub_volume") {
    if(fill) {
      scale <- scale_fill_continuous("m3/m2",..., na.value = NA)
    } else {
      scale <- scale_color_continuous("m3/m2",..., na.value = NA)
    }
  }
  else {
    if(fill) {
      scale <- scale_fill_continuous("value", ..., na.value = NA)
    } else {
      scale <- scale_color_continuous("value", ..., na.value = NA)
    }
  }
  return(scale)
}

#' Landscape variables
#' 
#' Extract or estimate variables from landscape objects (class 'sf').
#' 
#' @param x An object of class \code{\link[sf]{sf}} with the appropriate columns.
#' @param vars A string vector with the name of the variables to extract (see details).
#' @param variable A string with the name of the variables to draw (see details).
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}), required for most forest stand variables.
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' @param ... Additional arguments (not used).
#' 
#' @details The following string values are available for \code{vars}. 
#' 
#'  \emph{Topography}:
#'    \itemize{
#'       \item{\code{"elevation"}: Elevation in m.}
#'       \item{\code{"slope"}: Slope in degrees.} 
#'       \item{\code{"aspect"}: Slope in degrees.} 
#'       \item{\code{"land_cover_type"}: Land cover type.}
#'    }
#'    
#'  \emph{Soil}:
#'    \itemize{
#'      \item{\code{"soil_vol_extract"}: Total water extractable volume (mm).}
#'      \item{\code{"soil_vol_sat"}: Total water volume at saturation (mm).}
#'      \item{\code{"soil_vol_fc"}: Total water volume at field capacity (mm).}
#'      \item{\code{"soil_vol_wp"}: Total water volume at wilting point (mm).}
#'      \item{\code{"soil_vol_curr"}: Current total water volume (mm).}
#'      \item{\code{"soil_rwc_curr"}: Current soil relative water content (%).}
#'      \item{\code{"soil_rew_curr"}: Current soil relative extractable water (%).}
#'      \item{\code{"soil_theta_curr"}: Current soil moisture content (% vol.)}
#'      \item{\code{"soil_psi_curr"}: Current soil water potential (MPa).}
#'    }
#'    
#'  \emph{Watershed}:
#'    \itemize{
#'      \item{\code{"depth_to_bedrock"}: Depth to bedrock (m).}
#'      \item{\code{"bedrock_porosity"}: Bedrock porosity.}
#'      \item{\code{"bedrock_conductivity"}: Bedrock conductivity (m/day).}
#'      \item{\code{"aquifer_elevation"}: Aquifer elevation over bedrock (m).}
#'      \item{\code{"depth_to_aquifer"}: Depth to aquifer (m).}
#'      \item{\code{"aquifer"}: Aquifer volume (mm).}
#'      \item{\code{"snowpack"}: Snowpack water equivalent (mm).}
#'    }
#'
#' \emph{Forest stand}:
#'    \itemize{
#'      \item{\code{"basal_area"}: Basal area (m2/ha).}
#'      \item{\code{"tree_density"}: Tree density (ind/ha).}
#'      \item{\code{"mean_tree_height"}: Mean tree height (cm).}
#'      \item{\code{"dominant_tree_height"}: Dominant tree height (cm).}
#'      \item{\code{"dominant_tree_diameter"}: Dominant tree diameter (cm).}
#'      \item{\code{"quadratic_mean_tree_diameter"}: Quadratic mean tree diameter (cm).}
#'      \item{\code{"hart_becking_index"}: Hart-Becking index.}
#'      \item{\code{"leaf_area_index"}: Leaf area index (m2/m2).} 
#'      \item{\code{"foliar_biomass"}: Foliar biomass (kg/m2).} 
#'      \item{\code{"fuel_loading"}: Fine live fuel loading (kg/m2).} 
#'      \item{\code{"shrub_volume"}: Shrub volume (m3/m2).}
#'    }
#'
#' @returns Function \code{extract_variables()} returns an object of class \code{\link[sf]{sf}} with the desired variables.
#' Function \code{plot_variables()} returns a ggplot object.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link[medfate]{forest}}, \code{\link[medfate]{soil}}, \code{\link[medfate]{summary.forest}}, \code{\link{shinyplot_land}}
#' 
#' @examples
#' # Load data and species parameters from medfate
#' data(example_ifn)
#' data(SpParamsMED)
#'   
#' # Calculate basal area and leaf area index
#' # for all forest stands
#' extract_variables(example_ifn, vars = c("basal_area", "leaf_area_index"),
#'                   SpParams = SpParamsMED)
#'                   
#' @name extract_variables
#' @export
extract_variables<-function(x, vars = "land_cover_type", SpParams = NULL, ...) {
  if(!inherits(x, "sf"))  cli::cli_abort("'x' has to be of class 'sf'")
  df = sf::st_sf(geometry = sf::st_geometry(x))
  for(var in vars) {
    df[[var]] = .getLandscapeVar(x, var, SpParams, ...)
  }
  return(sf::st_as_sf(tibble::as_tibble(df)))
}

#' @rdname extract_variables
#' @export
plot_variable<-function(x, variable = "land_cover_type", SpParams = NULL, r = NULL, ...){
  df = extract_variables(x, vars= variable, SpParams = SpParams)
  character_var <- is.character(df[[variable]])
  if(is.null(r)) {
    g<-ggplot()+geom_sf(data=df, aes(col=.data[[variable]])) +
      .getDefaultScale(variable, fill = FALSE, ...)
  } else {
    sf_coords <- sf::st_coordinates(x)
    sf2cell <- terra::cellFromXY(r, sf_coords)
    if(any(is.na(sf2cell))) cli::cli_abort("Some coordinates are outside the raster definition.")
    if(length(sf2cell)!=length(unique(sf2cell))) cli::cli_abort("Only one element in 'sf' is allowed per cell in 'r'.")
    raster_var <- r
    m1 <- rep(NA, terra::ncell(r))
    m1[sf2cell] <- df[[variable]]
    raster_var$m1 <- m1
    g<-ggplot()+
      geom_spatraster(aes(fill=.data$m1), data = raster_var) +
      .getDefaultScale(variable, fill = TRUE, ...)
  }
  g+theme_bw()
}
