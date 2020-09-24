\encoding{UTF-8}
\name{buildWatershedTopography}
\alias{buildWatershedTopography}
\title{
Build SpatialPixelsTopography for watersheds
}
\description{
Defines objects \code{\link{SpatialPixelsTopography-class}} for watersheds whose boundaries are specified in a \code{\link{SpatialPolygons-class}} object.
}
\usage{
buildWatershedTopography(boundaries, grid, topo, proj4string, merge = FALSE)
}
\arguments{
  \item{boundaries}{An object \code{\link{SpatialPolygons-class}} or \code{\link{SpatialPolygonsDataFrame-class}} containing the boundaries of the watershed.}
  \item{grid}{An object \code{\link{GridTopology-class}} containing the grid definition.}
  \item{topo}{An object \code{\link{SpatialGridTopography-class}} containing the topographic features at appropriate resolution}
  \item{proj4string}{Projection string (i.e. \code{\link{CRS}}) corresponding to the grid definition.}
  \item{merge}{A boolean flag to indicate wether polygons should be merged.}
}
\value{
A list of objects \code{\link{SpatialPixelsTopography-class}}, each one describing the topography of the watershed. If \code{merge = TRUE} or the number of polygons is one, a single \code{\link{SpatialPixelsTopography-class}} object.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC.
}
\seealso{
\code{\link{SpatialPixelsTopography}}
}
\examples{
}
