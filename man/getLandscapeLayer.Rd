\encoding{UTF-8}
\name{getLandscapeLayer}
\alias{getLandscapeLayer}
\alias{getLandscapeVariable}
\title{Extract forest and soil data}
\description{
 Functions to extract spatial values from objects \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.
}
\usage{
getLandscapeLayer(obj, variable, SpParams = NULL, ...)
getLandscapeVariable(obj, variable, SpParams = NULL, ...)
}
\arguments{
  \item{obj}{An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.}
  \item{variable}{A string with the name of the variable to extract (see details).}
  \item{SpParams}{A data frame with species parameters (see \code{\link{SpParamsMED}}).}
  \item{...}{Additional arguments (not used).}
}
\details{
The following string values are available for \code{variable}. 

\emph{Topography}:
\itemize{
   \item{\code{"elevation"}:}{Elevation in m.}
   \item{\code{"slope"}:}{Slope in degrees.} 
   \item{\code{"aspect"}:}{Slope in degrees.} 
   \item{\code{"lct"}:}{Land cover type.}
}

\emph{Soil}:
\itemize{
   \item{\code{"texture1"}:}{Texture class of the first soil layer.}
   \item{\code{"texture2"}:}{Texture class of the second soil layer.} 
   \item{\code{"texture3"}:}{Texture class of the third soil layer.} 
   \item{\code{"SoilVolExtract"}:}{Total soil extractable volume (mm).}
}
}
\value{
An object of class \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialPixelsDataFrame}} or \code{\link{SpatialGridDataFrame}}, depending on the input.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}, \code{\link{shinyplotland}}
}
\examples{
library(medfate)

#Load plot data and species parameters from medfate
data(examplepointslandscape)
data(SpParamsMED)

#Apply summary function
y <- spatialForestSummary(examplepointslandscape,summary.forest, SpParamsMED)
head(y@data)

#Plot basal area
spplot(y["BA"])

}
