\encoding{UTF-8}
\name{SpatialPointsLandscape}
\alias{SpatialPointsLandscape}
\alias{SpatialPixelsLandscape}
\alias{SpatialGridLandscape}
\title{
Creates spatial landscape objects
}
\description{
Functions to instantiate spatial landscape classes.
}
\usage{
SpatialPointsLandscape(spt, lct, forestlist, soillist)
SpatialPixelsLandscape(spxt, lct, forestlist, soillist)
SpatialGridLandscape(sgt, lct, forestlist, soillist)
}
\arguments{
  \item{spt}{An object of class \code{\link{SpatialPointsTopography}}.}
  \item{spxt}{An object of class \code{\link{SpatialPixelsTopography}}.}
  \item{sgt}{An object of class \code{\link{SpatialGridTopography}}.}
  \item{lct}{A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock' or 'static').}
  \item{forestlist}{A list of objects of class 'forest' with the same number of elements as spatial points/pixels.}
  \item{soillist}{A list of objects of class 'forest' with the same number of elements as spatial points/pixels.}
}
\value{
An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}} depending on the function.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{\code{\link{forest}}, \code{\link{soil}}, \code{\link{DistributedWatershed}}}
