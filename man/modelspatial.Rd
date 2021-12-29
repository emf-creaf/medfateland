\encoding{UTF-8}
\name{spwbpoints}
\alias{spwbpoints}
\alias{spwbpixels}
\alias{spwbgrid}
\alias{growthpoints}
\alias{growthpixels}
\alias{growthgrid}
\alias{fordynpoints}
\alias{fordynpixels}
\alias{fordyngrid}

\title{Simulations for spatially-distributed forest stands}
\description{
Functions that allow calling local models \code{\link{spwb}}, \code{\link{growth}} or \code{\link{fordyn}}, for a set of forest stands distributed in specific locations. No spatial processes are simulated.
}
\usage{
spwbpoints(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
spwbpixels(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
spwbgrid(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
growthpoints(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
growthpixels(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
growthgrid(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
fordynpoints(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, managementFunction = NULL, managementArgs = NULL,
       keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
fordynpixels(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, managementFunction = NULL, managementArgs = NULL,
       keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
fordyngrid(y, SpParams, meteo, localControl = defaultControl(),
       dates = NULL, managementFunction = NULL, managementArgs = NULL,
       keepResults = TRUE, summaryFunction = NULL, summaryArgs=NULL, 
       parallelize = FALSE, numCores = detectCores()-1, chunk.size = NULL, 
       progress = TRUE)
}
\arguments{
  \item{y}{An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}, depending on the function called.}
  \item{SpParams}{A data frame with species parameters (see \code{\link{SpParamsMED}}).}
  \item{meteo}{Meteorology data (see details).}
  \item{localControl}{A list of local model control parameters (see \code{\link{defaultControl}}).}
  \item{dates}{A \code{\link{Date}} object with the days of the period to be simulated. If \code{NULL}, then the whole period of \code{meteo} is used.}
  \item{keepResults}{Boolean flag to indicate that point/cell simulation results are to be returned (set to \code{FALSE} and use summary functions for large data sets).}
  \item{summaryFunction}{An appropriate function to calculate summaries (e.g., \code{\link{summary.spwb}}).}
  \item{summaryArgs}{List with additional arguments for the summary function.}
  \item{parallelize}{Boolean flag to try parallelization (will use all clusters minus one).}
  \item{numCores}{Integer with the number of cores to be used for parallel computation.}
  \item{chunk.size}{Integer indicating the size of chuncks to be sent to different processes (by default, the number of spatial elements divided by the number of cores).}
  \item{progress}{Boolean flag to display progress information for simulations.}
  \item{managementFunction}{A function that implements forest management actions (see \code{\link{fordyn}}).}
  \item{managementArgs}{A list of arguments to be passed to the managementFunction (see \code{\link{fordyn}}).}
}
\details{
Simulation functions  accept different formats for meteorological input (parameter \code{meteo}). The user may supply four kinds of weather sources:
\enumerate{
\item{An object of \code{\link{SpatialPointsMeteorology-class}}, \code{\link{SpatialGridMeteorology-class}} or \code{\link{SpatialPixelsMeteorology-class}}, with the same spatial geometry as \code{y}.}
\item{An object of \code{\link{MeteorologyInterpolationData-class}}.}
\item{A data frame with meteorological data common for all points or grid cells.}
\item{A character vector with filenames from which to read weather data.}
}
In the case of (1) or (4), all the spatio-temporal variation of weather is already supplied by the user. In the case of (2), interpolation of weather is done for each spatial location and the period indicated in \code{meteo} or, if supplied, \code{dates}. Finally, in the case of (3) spatial variation of weather is not considered.
}
\value{
A list of class of the same name as the function called, also inheriting from an appropriate summary class (\code{summarypoints}, \code{summarypixels} or \code{summarygrid}), containing four elements:
\itemize{
\item{\code{sp}: An object with spatial information (of \code{SpatialPoints-class}, \code{SpatialPixels-class} or \code{SpatialGrid-class}).}
\item{\code{xlist}: A list of \code{\link{spwbInput}} or \code{\link{growthInput}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{updateState}}).}
\item{\code{forestlist}: A list of \code{\link{forest}} objects for each simulated stand (only in \code{fordynpoints}, \code{fordynpixels} and \code{fordyngrid}), to be used in subsequent simulations (see \code{\link{updateState}}).}
\item{\code{resultlist}: A list of model output for each simulated stand (if \code{keepResults = TRUE}).}
\item{\code{summarylist}: A list of model output summaries for each simulated stand (if \code{summaryFunction} is not \code{NULL}).}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{spwb}}, \code{\link{growth}}, \code{\link{fordyn}}, \code{\link{spwbpoints_day}}, \code{\link{summary.spwbpoints}} , \code{\link{plot.summarypoints}}, \code{\link{updateState}}
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
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
res = spwbpoints(examplepointslandscape, SpParamsMED, examplemeteo, dates = dates)

#Generate summaries (these could have also been specified when calling 'spwbpoints')
res_sum = summary(res, summaryFunction = summary.spwb, freq="month")

#Plot summaries
plot(res_sum, "Transpiration", "2001-03-01")
}
}