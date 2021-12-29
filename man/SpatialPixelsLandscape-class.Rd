\encoding{UTF-8}
\name{SpatialPixelsLandscape-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialPixelsLandscape-class}
\alias{plot,SpatialPixelsLandscape,ANY-method}
\alias{spplot,SpatialPixelsLandscape-method}
\alias{spatialForestSummary,SpatialPixelsLandscape-method}
\alias{spatialSoilSummary,SpatialPixelsLandscape-method}
\alias{getLandscapeLayer,SpatialPixelsLandscape-method}
\alias{getLandscapeVariable,SpatialPixelsLandscape-method}

\title{Class \code{"SpatialPixelsLandscape"}}
\description{
An S4 class that represents a landscape configuration over an (incomplete) grid of coordinates.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPixelsLandscape", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.}
    \item{\code{coords}:}{A matrix of pixel coordinates.}
    \item{\code{coords.nrs}:}{Inherited from \code{SpatialPointsDataFrame} but not used.}
    \item{\code{grid}:}{Object of class \code{GridTopology}. }
    \item{\code{grid.index}:}{Index of points in full grid.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}   
    \item{\code{lct}:}{A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
    \item{\code{forestlist}:}{Object of class \code{"list"} containing a set of \code{\link{forest}} objects.  }
    \item{\code{soillist}:}{Object of class \code{"list"} containing a set of \code{\link{soil}} objects.}
    \item{\code{xlist}:}{Object of class \code{"list"} containing a set of spwb or growth model input objects.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPixelsTopography}"}, directly.
Class \code{"\linkS4class{SpatialPixelsDataFrame}"}, distance 2.
Class \code{"\linkS4class{SpatialPixels}"}, distance 3.
Class \code{"\linkS4class{SpatialPointsDataFrame}"}, distance 3.
Class \code{"\linkS4class{SpatialPoints}"}, distance 4.
Class \code{"\linkS4class{Spatial}"}, distance 5.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "SpatialPixelsLandscape", y = "ANY")}: allows plotting ggplot maps of the landscape state.}
    \item{spplot}{\code{signature(obj = "SpatialPixelsLandscape")}: allows plotting maps of the landscape state.}
    \item{spatialForestSummary}{\code{signature(object = "SpatialPixelsLandscape")}: calculates a summary function for all forest stands and returns an object of class \code{\link{SpatialPixelsDataFrame-class}}. }
    \item{spatialSoilSummary}{\code{signature(object = "SpatialPixelsLandscape")}: calculates a summary function for the soil of all stands and returns an object of class \code{\link{SpatialPixelsDataFrame-class}}. }
    \item{getLandscapeLayer}{\code{signature(obj = "SpatialPixelsLandscape")}: allows retrieving a variable as SpatialPixelsDataFrame.}
    \item{getLandscapeVariable}{\code{signature(obj = "SpatialPixelsLandscape")}: allows retrieving a variable as vector.}
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{spatialForestSummary}}, \code{\link{forest}}, \code{\link{soil}}, \code{\link{getLandscapeLayer}}
}
\examples{
#Structure of the S4 object
showClass("SpatialPixelsLandscape")
}
\keyword{classes}
