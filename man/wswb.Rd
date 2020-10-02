\encoding{UTF-8}
\name{wswb}
\alias{wswb}
\title{Watershed water balance model}
\description{
A distributed hydrological model that simulates daily water balance on the cells of a watershed while accounting for overland runoff, subsurface flow and groundwater flow between cells.
}
\usage{
wswb(y, SpParams, meteo, dates,
     summaryFreq = "years",
     spwbcontrol = medfate::defaultControl(),
     correctionFactors = defaultWatershedCorrectionFactors())
}
\arguments{
  \item{y}{An object of class \code{\link{DistributedWatershed-class}}.}
  \item{SpParams}{A data frame with species parameters (see \code{\link{SpParamsMED}}).}
  \item{meteo}{Input meteorological data (see section details).}
  \item{dates}{A \code{\link{Date}} object describing the days of the period to be modeled.}
  \item{summaryFreq}{Frequency in which summary layers will be produced (e.g. "years", "months", ...) (see \code{\link{cut.Date}}).}
  \item{spwbcontrol}{A list of control parameters (see \code{\link{defaultControl}}) for function \code{\link{spwb}}.}
  \item{correctionFactors}{A list of watershed correction factors for hydraulic parameters.}
}
\details{
Function \code{wswb} requires daily meteorological data over grid cells. The user may supply four different inputs:
\enumerate{
\item{An object of \code{\link{SpatialPixelsMeteorology-class}}.}
\item{An object of \code{\link{MeteorologyInterpolationData-class}}.}
\item{A data frame with information regarding where to read meteorological data.}
\item{A data frame with meteorological data common for all cells of the grid.}
}
In the case of (1), all the spatio-temporal variation of weather is already supplied by the user. In the case of (2), interpolation of weather is done over each grid cell every simulated day. In the case of (3) weather maps are read for each day. Finally, in the case of (4) spatial variation of weather is not considered.
}
\value{
A list of class 'wswb' with the following elements:
\itemize{
 \item{\code{coords}: A matrix of pixel coordinates.}
 \item{\code{coords.nrs}: Inherited from \code{SpatialPointsDataFrame} but not used.}
 \item{\code{grid}: The \code{\link{GridTopology}} object corresponding to the simulated area.}
 \item{\code{grid.index}: Index of points in full grid.}
 \item{\code{bbox}: Object of class \code{"matrix"} with the boundary box.}
 \item{\code{WatershedBalance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level (i.e., rain, snow, interception, infiltration, soil evaporation, plant transpiration, ...).}
 \item{\code{WatershedSoilBalance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level restricted to those cells with a soil definition.}
 \item{\code{CellBalance}: A list of matrices (each with as many rows as cells and as many columns as summary points) representing the water balance for each cell:
   \itemize{
     \item{\code{Rain}: Rainfall (in mm).}
     \item{\code{Snow}: Snowfall (in mm).}
     \item{\code{Snowmelt}: Snow melt (in mm).}
     \item{\code{Interception}: Rainfall interception (in mm).}
     \item{\code{NetRain}: Net rain, i.e. throughfall, (in mm).}
     \item{\code{Runon}: The amount of water imported from other cells via surface runoff (in mm).}
     \item{\code{Runoff}: The amount of water exported via surface runoff (in mm).}
     \item{\code{Infiltration}: The amount of water infiltrating into the soil (in mm).}
     \item{\code{DeepDrainage}: The amount of water draining from soil to the aquifer via deep drainage (in mm).}
     \item{\code{SaturationExcess}: The amount of water that reaches the soil surface because of soil saturation (in mm).}
     \item{\code{AquiferDischarge}: The amount of water that reaches deepest soil layer from a saturated aquifer (in mm).}
     \item{\code{SubsurfaceInput}: The amount of water that reaches the soil from adjacent cells via subsurface flow (in mm).}
     \item{\code{SubsurfaceOutput}: The amount of water that leaves the soil towards adjacent cells via subsurface flow (in mm).}
     \item{\code{GroundwaterInput}: The amount of water that reaches the aquifer from adjacent cells via groundwater flow (in mm).}
     \item{\code{GroundwaterOutput}: The amount of water that leaves the aquifer towards adjacent cells via groundwater flow (in mm).}
     \item{\code{SoilEvaporation}: Bare soil evaporation (in mm).}
     \item{\code{Transpiration}: Plant transpiration (in mm).}
  }
 }
 \item{\code{CellState}: A list of matrices (each with as many rows as cells and as many columns as summary points) representing the average soil state for each cell:
   \itemize{
     \item{\code{SWE}: Snow water equivalent (in mm) of the snowpack.}
     \item{\code{Psi1}: Soil water potential of the topmost layer (in MPa).}
     \item{\code{SoilVol}: Soil water volume integrated across vertical layers (in mm).}
     \item{\code{WTD}: Water table depth (in mm from surface).}
  }
 }
 \item{\code{DailyRunoff}: A matrix with daily runoff (in m3/day) at each of the outlet cells of the landscape.}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC.
}
\seealso{
\code{\link{plot.wswb}}, \code{\link{spwb}}, \code{\link{spwbpoints}}, \code{\link{DistributedWatershed-class}}
}
\examples{
\dontrun{
data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")

dates = seq(as.Date("2001-01-01"), as.Date("2001-03-31"), by="day")

res = wswb(examplewatershed, SpParamsMED, examplemeteo, dates = dates, summaryFreq = "month")
}
}
