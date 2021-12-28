\encoding{UTF-8}
\name{getLandscapeLayer}
\alias{getLandscapeLayer}
\alias{getLandscapeVariable}
\title{Extract forest and soil data}
\description{
 Functions to extract spatial values from objects \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.
}
\usage{
getLandscapeLayer(obj, variable, ...)
getLandscapeVariable(obj, variable, ...)
}
\arguments{
  \item{obj}{An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}.}
  \item{variable}{A string with the name of the variable to extract.}
  \item{...}{Additional arguments (not used).}
}
\value{
An object of class \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialPixelsDataFrame}} or \code{\link{SpatialGridDataFrame}}, depending on the input.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{forest}}, \code{\link{soil}}, \code{\link{summary.forest}}
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
