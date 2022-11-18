#' sp to sf conversion
#' 
#' Converts objects of older (deprecated) classes to sf
#' 
#' @param y An object of class 'Spatial_*_Landscape' or 'DistributedWatershed'
#' 
#' @return An object of class 'sf'
#' 
sp_to_sf<-function(y) {
  epl = sf::st_as_sf(as(y, "SpatialPoints"))
  ids = row.names(coordinates(y))
  epl$id = 1:nrow(epl)
  if(!is.null(ids)) epl$id = ids
  row.names(epl)<-NULL
  epl$elevation = y@data$elevation
  epl$slope = y@data$slope
  epl$aspect = y@data$aspect
  epl$landcovertype = y@lct
  forest_list = y@forestlist
  soil_list = y@soillist
  x_list = y@xlist
  names(forest_list) = NULL
  names(soil_list) = NULL
  names(x_list) = NULL
  epl$forest = forest_list
  epl$soil = soil_list
  epl$state = x_list
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
    epl$representedarea = prod(y@grid@cellsize)
  } else {
    epl$representedarea = NA
  }
  epl$managementunit = NA
  epl$managementarguments = vector("list", nrow(epl))
  return(sf::st_as_sf(tibble::as_tibble(epl)))
}