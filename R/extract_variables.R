.getAllowedTopographyVars <-function() {
  return(c( "Elevation (m)" = "elevation", 
            "Slope (degrees)" = "slope", 
            "Aspect (degrees)" = "aspect", 
            "Land cover type" = "land_cover_type"))
}
.getLandscapeTopographyVar<-function(obj, variable) {
  if(!(variable %in% names(obj))) stop(paste0("Object does not have a '", variable, "' column."))
  varplot = obj[[variable]]
  return(varplot)
}

.getAllowedSoilVars <-function() {
  return(c("Texture (first layer)" = "texture1", 
           "Texture (second layer)" = "texture2", 
           "Texture (third layer)" ="texture3",
           "Total water extractable volume (mm)" = "soilvolextract",
           "Total water volume at saturation (mm)" = "soilvolsat",
           "Total water volume at field capacity (mm)" = "soilvolfc",
           "Total water volume at wilting point (mm)" = "soilvolwp",
           "Current total water volume (mm)" = "soilvolcurr",
           "Soil moisture (first layer)" = "theta1",
           "Soil moisture (second layer)" = "theta2",
           "Soil moisture (third layer)" = "theta3",
           "Water potential (first layer)" = "psi1",
           "Water potential (second layer)" = "psi2",
           "Water potential (third layer)" = "psi3"))
}
.getLandscapeSoilVar<-function(obj, variable) {
  if(!("soil" %in% names(obj))) stop("Object does not have a 'soil' column.")
  n = length(obj$soil)
  varplot = rep(NA, n)
  for(i in 1:n) {
    s = obj$soil[[i]]
    if(inherits(s,"soil")) {
      if(variable=="texture1") varplot[i] = soil_USDAType(s$clay[1],s$sand[1])
      else if(variable=="texture2") varplot[i] = soil_USDAType(s$clay[2],s$sand[2])
      else if(variable=="texture3") varplot[i] = soil_USDAType(s$clay[3],s$sand[3])
      else if(variable=="soilvolextract") varplot[i] = sum(soil_waterExtractable(s), na.rm=TRUE)
      else if(variable=="soilvolsat") varplot[i] = sum(soil_waterSAT(s), na.rm=TRUE)
      else if(variable=="soilvolfc") varplot[i] = sum(soil_waterFC(s), na.rm=TRUE)
      else if(variable=="soilvolwp") varplot[i] = sum(soil_waterWP(s), na.rm=TRUE)
      else if(variable=="soilvolcurr") varplot[i] = sum(soil_water(s), na.rm=TRUE)
      else if(variable=="theta1") varplot[i] = soil_theta(s)[1]
      else if(variable=="theta2") varplot[i] = soil_theta(s)[2]
      else if(variable=="theta3") varplot[i] = soil_theta(s)[3]
      else if(variable=="psi1") varplot[i] = soil_psi(s)[1]
      else if(variable=="psi2") varplot[i] = soil_psi(s)[2]
      else if(variable=="psi3") varplot[i] = soil_psi(s)[3]
    }
  }
  return(varplot)
}

.getAllowedWatershedVars <-function(){
  return(c("Depth to bedrock (m)" = "depth_to_bedrock",
           "Bedrock porosity" = "bedrock_porosity", 
           "Bedrock conductivity" = "bedrock_conductivity",
           "Aquifer elevation (m)" = "aquifer_elevation", 
           "Depth to aquifer (m)" = "depth_to_aquifer",
           "Aquifer volume (mm)" = "aquifer_volume", 
           "Snowpack water equivalent (mm)" = "snowpack"))
}
.getLandscapeWatershedVar<-function(obj, variable) {
  if(variable=="num_neigh") {
    varplot = sapply(obj$queenNeigh,"length")
  } else if(variable=="water_order") {
    wo = obj$waterOrder
    varplot = 1:length(wo)
    varplot[wo] = 1:length(wo)
  } else if(variable=="outlets") {
    outlets = which(unlist(lapply(obj$waterQ, sum))==0)
    varplot = rep(FALSE, length(obj$waterQ))
    varplot[outlets] = TRUE
  } else if(variable =="depth_to_bedrock") {
    varplot = obj$depth_to_bedrock/1000.0  # in m
  } else if(variable =="bedrock_porosity") {
    varplot = obj$bedrock_porosity
  } else if(variable =="bedrock_conductivity") {
    varplot = obj$bedrock_conductivity
  } else if(variable =="aquifer_volume") {
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

.getAllowedForestStandVars <-function(SpParams = NULL) {
  vars <- c("Basal area (m2/ha)"="basal_area")
  if(!is.null(SpParams)) {
    vars <- c(vars,
              "Leaf area index (m2/m2)" = "leaf_area_index", 
              "Foliar biomass (kg/m2)" = "foliar_biomass", 
              "Fine live fuel (kg/m2)" = "fuel_loading")
  }
  return(vars)
}
.getLandscapeForestStandVar<-function(obj, variable, SpParams = NULL) {
  if(!("forest" %in% names(obj))) stop("Object does not have a 'forest' column.")
  n = length(obj$forest)
  varplot = rep(NA, n)
  for(i in 1:n) {
    f = obj$forest[[i]]
    if(inherits(f,"forest")) {
      if(variable=="basal_area") varplot[i] = stand_basalArea(f)
      else if(variable=="leaf_area_index") varplot[i] = stand_LAI(f, SpParams)
      else if(variable=="foliar_biomass") varplot[i] = stand_foliarBiomass(f, SpParams)
      else if(variable=="fuel_loading") varplot[i] = stand_fuelLoading(f, SpParams)
    }
  }
  return(varplot)
}

.getAllowedVars <-function(y, SpParams = NULL) {
  vars = character(0)
  vars = c(vars, .getAllowedTopographyVars(),.getAllowedSoilVars(),.getAllowedForestStandVars(SpParams))
  if("depth_to_bedrock" %in% names(y)) {
    vars = c(vars, .getAllowedWatershedVars())
  }
  return(vars)
}
.getLandscapeVar<-function(obj, variable, SpParams = NULL, ...) {
  variable = match.arg(variable, .getAllowedVars(obj, SpParams))
  if(variable %in% .getAllowedTopographyVars()) return(.getLandscapeTopographyVar(obj, variable))
  else if(variable %in% .getAllowedSoilVars()) return(.getLandscapeSoilVar(obj, variable))
  else if(variable %in% .getAllowedWatershedVars()) return(.getLandscapeWatershedVar(obj, variable))
  else if(variable %in% .getAllowedForestStandVars(SpParams)) return(.getLandscapeForestStandVar(obj, variable, SpParams))
}


#' Landscape variables
#' 
#' Extract or estimate variables from landscape objects (class 'sf').
#' 
#' @param x An object of class \code{\link{sf}} with the appropriate columns.
#' @param vars A string vector with the name of the variables to extract (see details).
#' @param variable A string with the name of the variables to draw (see details).
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}), required for most forest stand variables.
#' @param ... Additional arguments (not used).
#' 
#' @details The following string values are available for \code{vars}. 
#'  \emph{Topography}:
#'    \itemize{
#'       \item{\code{"elevation"}:}{Elevation in m.}
#'       \item{\code{"slope"}:}{Slope in degrees.} 
#'       \item{\code{"aspect"}:}{Slope in degrees.} 
#'       \item{\code{"land_cover_type"}:}{Land cover type.}
#'    }
#'    
#'  \emph{Soil}:
#'    \itemize{
#'      \item{\code{"texture1"}:}{Texture class of the first soil layer.}
#'      \item{\code{"texture2"}:}{Texture class of the second soil layer.} 
#'      \item{\code{"texture3"}:}{Texture class of the third soil layer.} 
#'      \item{\code{"soilvolextract"}:}{Total water extractable volume (mm).}
#'      \item{\code{"soilvolsat"}:}{Total water volume at saturation (mm).}
#'      \item{\code{"soilvolfc"}:}{Total water volume at field capacity (mm).}
#'      \item{\code{"soilvolwp"}:}{Total water volume at wilting point (mm).}
#'      \item{\code{"soilvolcurr"}:}{Current total water volume (mm).}
#'    }
#'    
#'  \emph{Watershed}:
#'    \itemize{
#'      \item{\code{"depth_to_bedrock"}:}{Depth to bedrock (m).}
#'      \item{\code{"bedrock_porosity"}:}{Bedrock porosity.}
#'      \item{\code{"bedrock_conductivity"}:}{Bedrock conductivity.}
#'      \item{\code{"aquifer_elevation"}:}{Aquifer elevation over bedrock (m).}
#'      \item{\code{"depth_to_aquifer"}:}{Depth to aquifer (m).}
#'      \item{\code{"aquifer_volume"}:}{Aquifer volume (mm).}
#'      \item{\code{"snowpack"}:}{Snowpack water equivalent (mm).}
#'    }
#'
#' \emph{Forest stand}:
#'    \itemize{
#'      \item{\code{"basal_area"}:}{Basal area (m2/ha).}
#'      \item{\code{"leaf_area_index"}:}{Leaf area index (m2/m2).} 
#'      \item{\code{"foliar_biomass"}:}{Foliar biomass (kg/m2).} 
#'      \item{\code{"fuel_loading"}:}{Fine live fuel loading (kg/m2).} 
#'      \item{\code{"shrub_volume"}:}{Shrub shrub_volume (m3/m2).}
#'    }
#'
#' @returns An object of class \code{\link{sf}} with the desired variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}, \code{\link{shinyplot_land}}
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
  if(!inherits(x, "sf")) stop("'x' has to be of class 'sf'")
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
  if(is.null(r)) {
    g<-ggplot()+geom_sf(data=df, aes(col=.data[[variable]]))+
      scale_color_continuous("", ..., na.value=NA)+
      theme_bw()
  } else {
    raster_var<-terra::rasterize(terra::vect(df),r, variable, fun = mean, na.rm = TRUE)
    names(raster_var) <- "m1"
    g<-ggplot()+
      geom_spatraster(aes(fill=m1), data = raster_var)+
      scale_fill_continuous("", ..., na.value=NA)+
      theme_bw()
  }
  g
}
