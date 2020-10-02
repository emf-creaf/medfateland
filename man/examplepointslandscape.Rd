\encoding{UTF-8}
\name{examplepointslandscape}
\alias{examplepointslandscape}
\docType{data}
\title{
Example of SpatialPointsLandscape
}
\description{
An example of an object of \code{\link{SpatialPointsLandscape-class}} with data corresponding to 30 forest plots.
}
\usage{data("examplepointslandscape")}
\format{
  The data format is that of an object \code{\link{SpatialPointsLandscape-class}} 
}
\source{
\itemize{
\item{Soil data from SoilGrids global database (Hengl et al. 2017).}
\item{Soil depth and depth to bedrock from Shangguan et al. (2017).}
\item{Forest structure and composition from the Third Spanish Forest Inventory (IFN3).}
}
}
\seealso{ 
\code{\link{SpatialPointsLandscape}}, \code{\link{SpatialPointsLandscape-class}}}
\examples{
data(examplepointslandscape)

plot(examplepointslandscape)
}
\keyword{datasets}
