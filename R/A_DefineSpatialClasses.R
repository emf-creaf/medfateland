#' Class \code{"SpatialPointsLandscape"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An S4 class that represents a set of forest stands along with their spatial location
#' 
#' @param data Object of class "data.frame" containing elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.
#' @param coords Object of class \code{"matrix"} with spatial coordinates.
#' @param bbox Object of class \code{"matrix"} with the boundary box.
#' @param proj4string Object of class \code{"CRS"} with the projection string.
#' @param lct A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').
#' @param forestlist Object of class \code{"list"} containing a set of \code{\link{forest}} objects.
#' @param soillist Object of class \code{"list"} containing a set of \code{\link{soil}} objects.
#' @param xlist Object of class \code{"list"} containing a set of spwb or growth model input objects.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}
#' 
#' @examples 
#' showClass("SpatialPointsLandscape")
#' 
setClass("SpatialPointsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialPointsTopography")

#' Class \code{"SpatialGridLandscape"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' An S4 class that represents a landscape configuration over a grid of coordinates.
#' 
#' @slot grid Object of class \code{"GridTopology"}.
#' @slot bbox Object of class \code{"matrix"} with the boundary box.
#' @slot proj4string Object of class \code{"CRS"} with the projection string.
#' @slot data Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.
#' @slot lct A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').
#' @slot forestlist Object of class \code{"list"} containing a set of \code{\link{forest}} objects.
#' @slot soillist Object of class \code{"list"} containing a set of \code{\link{soil}} objects.
#' @slot xlist Object of class \code{"list"} containing a set of spwb or growth model input objects.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}
#' 
#' @examples 
#' #Structure of the S4 object
#' showClass("SpatialGridLandscape")
#' 
setClass("SpatialGridLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialGridTopography")

#' Class \code{"SpatialPixelsLandscape"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' An S4 class that represents a landscape configuration over an (incomplete) grid of coordinates.
#' 
#' @param data Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.
#' @param coords A matrix of pixel coordinates.
#' @param coords.nrs Inherited from \code{SpatialPointsDataFrame} but not used.
#' @param grid Object of class \code{GridTopology}.
#' @param grid.index Index of points in full grid.
#' @param bbox Object of class \code{"matrix"} with the boundary box.
#' @param proj4string Object of class \code{"CRS"} with the projection string.
#' @param lct A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').
#' @param forestlist Object of class \code{"list"} containing a set of \code{\link{forest}} objects.
#' @param soillist Object of class \code{"list"} containing a set of \code{\link{soil}} objects.
#' @param xlist Object of class \code{"list"} containing a set of spwb or growth model input objects.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{forest}}, \code{\link{soil}}
#' 
#' @examples 
#' #Structure of the S4 object
#' showClass("SpatialPixelsLandscape")
#' 
setClass("SpatialPixelsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialPixelsTopography")


#' Class \code{"DistributedWatershed"}
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' An S4 class that represents a landscape configuration over a grid of coordinates.
#' 
#' @slot coords A matrix of pixel coordinates.
#' @slot coords.nrs Inherited from \code{SpatialPointsDataFrame} but not used.
#' @slot grid Object of class \code{"GridTopology"}.
#' @slot grid.index Index of points in full grid.
#' @slot bbox Object of class \code{"matrix"} with the boundary box.
#' @slot proj4string Object of class \code{"CRS"} with the projection string.
#' @slot data Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.
#' @slot lct A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').
#' @slot waterOrder A numeric vector of cell processing order.
#' @slot waterQ A list of water discharge values to neighbours.
#' @slot queenNeigh A list of neighbours for each cell.
#' @slot channel A logical vector indicating whether each cell belongs to the channel network.
#' @slot bedrock A data frame of bedrock properties (depth to bedrock, conductivity and porosity).
#' @slot aquifer A numeric vector with the water content of the aquifer in each cell.
#' @slot snowpack A numeric vector with the snow water equivalent content of the snowpack in each cell.
#' @slot forestlist Object of class \code{"list"} containing a set of \code{\link{forest}} objects.
#' @slot soillist Object of class \code{"list"} containing a set of \code{\link{soil}} objects.
#' @slot xlist Object of class \code{"list"} containing a set of spwb or growth model input objects.
#' 
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{SpatialPixelsTopography-class}}, \code{\link{SpatialPixelsLandscape-class}}
#' 
#' @examples
#' #Structure of the S4 object
#' showClass("DistributedWatershed")
#' 
setClass("DistributedWatershed",
         slots=list(waterOrder = "numeric", waterQ = "list", queenNeigh = "list",
                    channel = "logical", bedrock = "data.frame", snowpack = "numeric", aquifer = "numeric"),
         contains="SpatialPixelsLandscape")
