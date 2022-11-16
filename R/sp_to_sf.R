#' sp to sf conversion
#' 
#' Converts objects of older classes to sf
#' 
#' @param y An object of class 'Spatial_*_Landscape' or 'DistributedWatershed'
#' 
#' @return An object of class 'sf'
#' 
sp_to_sf<-function(y) {
  epl = sf::st_as_sf(as(y, "SpatialPoints"))
  epl$id = row.names(coordinates(y))
  row.names(epl)<-NULL
  epl$elevation = y@data$elevation
  epl$slope = y@data$slope
  epl$aspect = y@data$aspect
  epl$landcovertype = y@lct
  forest_list = y@forestlist
  soil_list = y@soillist
  names(forest_list) = NULL
  names(soil_list) = NULL
  epl$forest = forest_list
  epl$soil = soil_list
  if(inherits(y,"DistributedWatershed")) {
    epl$waterOrder = y@waterOrder
    epl$waterQ = y@waterQ
    epl$queenNeigh = y@queenNeigh
    epl$channel = y@channel
    epl$depthtobedrock = y@bedrock$DepthToBedrock
    epl$bedrockconductivity = y@bedrock$Conductivity
    epl$bedrockporosity = y@bedrock$Porosity
    epl$snowpack = y@snowpack
    epl$aquifer = y@aquifer
  }
  epl$managementunit = NA
  epl$managementarguments = NA
  epl$representedarea = NA
  return(sf::st_as_sf(tibble::as_tibble(epl)))
}