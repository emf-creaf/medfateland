.getAllowedTopographyVars <-function() {
  return(c( "Elevation (m)" = "elevation", 
            "Slope (degrees)" = "slope", 
            "Aspect (degrees)" = "aspect", 
            "Land cover type" = "landcovertype"))
}
.getLandscapeTopographyVar<-function(obj, variable) {
  if(variable=="landcovertype") {
    varplot = obj$landcovertype
  } else if(variable=="elevation") {
    varplot = obj$elevation
  } else if(variable=="slope") {
    varplot = obj$slope
  } else if(variable=="aspect") {
    varplot = obj$aspect
  }
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
  return(c("Number of cell neighbours" = "numneigh", 
           "Cell order for lateral water transfer" = "waterorder", 
           "Water outlet (binary)" = "outlets", 
           "Water channel (binary)" = "channel",
           "Depth to bedrock (m)" = "depthtobedrock",
           "Bedrock porosity" = "bedrockporosity", 
           "Bedrock conductivity" = "bedrockconductivity",
           "Aquifer elevation (m)" = "aquiferelevation", 
           "Depth to aquifer (m)" = "depthtoaquifer",
           "Aquifer volume (mm)" = "aquifervolume", 
           "Snowpack water equivalent (mm)" = "snowpack"))
}
.getLandscapeWatershedVar<-function(obj, variable) {
  if(variable=="numneigh") {
    varplot = sapply(obj$queenNeigh,"length")
  } else if(variable=="waterorder") {
    wo = obj$waterOrder
    varplot = 1:length(wo)
    varplot[wo] = 1:length(wo)
  } else if(variable=="outlets") {
    outlets = which(unlist(lapply(obj$waterQ, sum))==0)
    varplot = rep(FALSE, length(obj$waterQ))
    varplot[outlets] = TRUE
  } else if(variable=="channel") {
    varplot = obj$channel
  } else if(variable =="depthtobedrock") {
    varplot = obj$depthtobedrock/1000.0  # in m
  } else if(variable =="bedrockporosity") {
    varplot = obj$bedrockporosity
  } else if(variable =="bedrockconductivity") {
    varplot = obj$bedrockconductivity
  } else if(variable =="aquifervolume") {
    varplot = obj$aquifer
  } else if(variable =="aquiferelevation") {
    DTB = obj$depthtobedrock
    aquifer = obj$aquifer
    RockPorosity = obj$bedrockporosity
    elevation = obj$elevation
    varplot = elevation - (DTB/1000.0) + (aquifer/RockPorosity)/1000.0 # in m
    varplot[RockPorosity==0.0] = elevation[RockPorosity==0.0]
  } else if(variable=="snowpack") {
    varplot = obj$snowpack
  } else if(variable =="depthtoaquifer") {
    DTB = obj$depthtobedrock
    aquifer = obj$aquifer
    RockPorosity = obj$bedrockporosity
    varplot = (DTB/1000.0) - (aquifer/RockPorosity)/1000.0
    varplot[RockPorosity==0.0] = DTB[RockPorosity==0.0]/1000
  }
  return(varplot)
}

.getAllowedForestStandVars <-function(SpParams = NULL) {
  vars <- c("Basal area (m2/ha)"="basalarea")
  if(!is.null(SpParams)) {
    vars <- c(vars,
              "Leaf area index (m2/m2)" = "leafareaindex", 
              "Foliar biomass (kg/m2)" = "foliarbiomass", 
              "Fine live fuel (kg/m2)" = "fuel", 
              "Shrub phytovolume (m3/m2)" = "phytovolume")
  }
  return(vars)
}
.getLandscapeForestStandVar<-function(obj, variable, SpParams = NULL) {
  n = length(obj$forest)
  varplot = rep(NA, n)
  for(i in 1:n) {
    f = obj$forest[[i]]
    if(inherits(f,"forest")) {
      if(variable=="basalarea") varplot[i] = stand_basalArea(f)
      else if(variable=="leafareaindex") varplot[i] = stand_LAI(f, SpParams)
      else if(variable=="foliarbiomass") varplot[i] = stand_foliarBiomass(f, SpParams)
      else if(variable=="fuel") varplot[i] = stand_fuel(f, SpParams)
      else if(variable=="phytovolume") varplot[i] = stand_phytovolume(f, SpParams)
    }
  }
  return(varplot)
}

.getAllowedVars <-function(y, SpParams = NULL) {
  vars = character(0)
  vars = c(vars, .getAllowedTopographyVars(),.getAllowedSoilVars(),.getAllowedForestStandVars(SpParams))
  if("depthtobedrock" %in% names(y)) {
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
#'       \item{\code{"landcovertype"}:}{Land cover type.}
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
#'      \item{\code{"numneigh"}:}{Number of cell neighbours (integer).}
#'      \item{\code{"waterorder"}:}{Cell order for lateral water transfer (integer).}
#'      \item{\code{"outlets"}:}{Water outlet (TRUE/FALSE).}
#'      \item{\code{"channel"}:}{Water channel (TRUE/FALSE).}
#'      \item{\code{"depthTobedrock"}:}{Depth to bedrock (m).}
#'      \item{\code{"bedrockporosity"}:}{Bedrock porosity.}
#'      \item{\code{"bedrockconductivity"}:}{Bedrock conductivity.}
#'      \item{\code{"aquiferelevation"}:}{Aquifer elevation over bedrock (m).}
#'      \item{\code{"depthtoaquifer"}:}{Depth to aquifer (m).}
#'      \item{\code{"aquifervolume"}:}{Aquifer volume (mm).}
#'      \item{\code{"snowpack"}:}{Snowpack water equivalent (mm).}
#'    }
#'
#' \emph{Forest stand}:
#'    \itemize{
#'      \item{\code{"basalarea"}:}{Basal area (m2/ha).}
#'      \item{\code{"leafareaindex"}:}{Leaf area index (m2/m2).} 
#'      \item{\code{"foliarbiomass"}:}{Foliar biomass (kg/m2).} 
#'      \item{\code{"fuel"}:}{Fine live fuel (kg/m2).} 
#'      \item{\code{"phytovolume"}:}{Shrub phytovolume (m3/m2).}
#'    }
#'
#' @returns An object of class \code{\link{sf}} with the desired variables.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}, \code{\link{shinyplotland}}
#' 
#' @examples
#' # Load data and species parameters from medfate
#' data(examplepointslandscape)
#' data(SpParamsMED)
#'   
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplepointslandscape)
#'  
#' # Calculate basal area and leaf area index
#' # for all forest stands
#' extract_variables(y, vars = c("basalarea", "leafareaindex"),
#'                   SpParams = SpParamsMED)
#'                   
#' @name extract_variables
extract_variables<-function(x, vars = "landcovertype", SpParams = NULL, ...) {
  df = sf::st_sf(geometry = sf::st_geometry(x))
  for(var in vars) {
    df[[var]] = .getLandscapeVar(x, var, SpParams, ...)
  }
  return(sf::st_as_sf(tibble::as_tibble(df)))
}

#' @rdname extract_variables
plot_variable<-function(x, variable = "landcovertype", SpParams = NULL, ...){
  df = extract_variables(x, vars= variable, SpParams = SpParams)
  g<-ggplot()+geom_sf(data=df, aes(col=.data[[variable]]))+
     theme_bw()
  g
}