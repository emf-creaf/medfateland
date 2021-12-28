\encoding{UTF-8}
\name{SpatialGridLandscape-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialGridLandscape-class}
\alias{plot,SpatialGridLandscape,ANY-method}
\alias{spplot,SpatialGridLandscape-method}
\alias{spatialForestSummary,SpatialGridLandscape-method}
\alias{spatialSoilSummary,SpatialGridLandscape-method}
\alias{getLandscapeLayer,SpatialGridLandscape-method}
\alias{getLandscapeVariable,SpatialGridLandscape-method}

\title{Class \code{"SpatialGridLandscape"}}
\description{
An S4 class that represents a landscape configuration over a grid of coordinates.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialGridLandscape", ...)}, or by calls to the function \code{\link{SpatialGridLandscape}}.
}
\section{Slots}{
  \describe{
    \item{\code{grid}:}{Object of class \code{"GridTopology"}. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}   
    \item{\code{data}:}{Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.}
    \item{\code{lct}:}{A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
    \item{\code{forestlist}:}{Object of class \code{"list"} containing a set of \code{\link{forest}} objects.  }
    \item{\code{soillist}:}{Object of class \code{"list"} containing a set of \code{\link{soil}} objects.}
    \item{\code{xlist}:}{Object of class \code{"list"} containing a set of spwb or growth model input objects.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialGridTopography}"}, directly.
Class \code{"\linkS4class{SpatialGridDataFrame}"}, distance 2.
Class \code{"\linkS4class{SpatialGrid}"}, distance 3.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialGrid", distance 4.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "SpatialGridLandscape", y = "ANY")}: allows plotting ggplot maps of the landscape state.}
    \item{spplot}{\code{signature(obj = "SpatialGridLandscape")}: allows plotting maps of the landscape state.}
    \item{spatialForestSummary}{\code{signature(object = "SpatialGridLandscape")}: calculates a summary function for all forest stands and returns an object of class \code{\link{SpatialGridDataFrame-class}}. }
    \item{spatialSoilSummary}{\code{signature(object = "SpatialGridLandscape")}: calculates a summary function for the soil of all stands and returns an object of class \code{\link{SpatialGridDataFrame-class}}. }
    \item{getLandscapeLayer}{\code{signature(object = "SpatialGridLandscape")}: allows retrieving a variable as SpatialPixelsDataFrame.}
    \item{getLandscapeVariable}{\code{signature(object = "SpatialGridLandscape")}: allows retrieving a variable as vector.}
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialGridTopography-class}}, \code{\link{SpatialPointsLandscape-class}}, \code{\link{spatialForestSummary}}, \code{\link{forest}}, \code{\link{soil}}
}
\examples{
#Structure of the S4 object
showClass("SpatialGridLandscape")
}
\keyword{classes}
