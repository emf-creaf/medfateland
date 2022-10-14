\encoding{UTF-8}
\name{spwbspatial_day}
\alias{spwbspatial_day}
\alias{growthspatial_day}

\title{One-day simulation for spatially-distributed forest stands}
\description{
Functions that allow calling local models \code{\link{spwb_day}} or \code{\link{growth_day}}, for a set of forest stands distributed in specific locations and a given date. No spatial processes are simulated.
}
\usage{
spwbspatial_day(y, meteo, date, SpParams, localControl = defaultControl(),
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
growthspatial_day(y, meteo, date, SpParams, localControl = defaultControl(),
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
}
\arguments{
  \item{y}{An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.}
  \item{meteo}{Meteorology data (see \code{\link{spwbspatial}}).}
  \item{date}{A string with the date to be simulated.}
  \item{SpParams}{A data frame with species parameters (see \code{\link{SpParamsMED}}).}
  \item{localControl}{A list of local model control parameters (see \code{\link{defaultControl}}).}
  \item{parallelize}{Boolean flag to try parallelization (will use all clusters minus one).}
  \item{numCores}{Integer with the number of cores to be used for parallel computation.}
  \item{chunk.size}{Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).}
  \item{progress}{Boolean flag to display progress information for simulations.}
}
\details{
Simulation functions accept different formats for meteorological input (parameter \code{meteo}) as described in \code{\link{spwbspatial}}
}
\value{
A list of class of the same name as the function called containing three elements:
\itemize{
\item{\code{sp}: An object with spatial information (of \code{SpatialPoints-class}, \code{SpatialPixels-class} or \code{SpatialGrid-class}).}
\item{\code{xlist}: A list of model input objects for each simulated stand, to be used in subsequent simulations.}
\item{\code{resultlist}: A list of model output for each simulated stand.}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{spwb_day}}, \code{\link{growth_day}}, \code{\link{spwbspatial}}
}
\examples{

\dontrun{
#Load example watershed (inherits from SpatialPixelsLandscape)
data("examplepointslandscape")

#Load example meteo data frame from package meteoland
data("examplemeteo")

#Load default medfate parameters
data("SpParamsMED")

#Perform simulation
date = "2001-03-01"
res = spwbspatial_day(examplepointslandscape, examplemeteo, date, SpParamsMED)
}
}