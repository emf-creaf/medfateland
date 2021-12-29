\name{updateState}
\alias{updateState}
\title{
Updates the state of a spatial object
}
\description{
Updates the state of a spatial object 'x' according to the final state in simulation outcome 'y' 
}
\usage{
updateState(x, y)
}
\arguments{
  \item{x}{An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}}
  \item{y}{The object resulting of a simulation previously carried on \code{x}.}
}
\value{
An object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}} with modified state variables.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbpoints}}, \code{\link{spwbpoints_day}}, \code{\link{spwbland}}
}
