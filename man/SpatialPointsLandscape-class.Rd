\encoding{UTF-8}
\name{SpatialPointsLandscape-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialPointsLandscape-class}
\alias{show,SpatialPointsLandscape-method}
\alias{head,SpatialPointsLandscape-method}
\alias{tail,SpatialPointsLandscape-method}
\alias{print,SpatialPointsLandscape-method}
\alias{[,SpatialPointsLandscape,ANY,ANY,ANY-method}
\alias{plot,SpatialPointsLandscape,ANY-method}
\alias{spplot,SpatialPointsLandscape-method}
\alias{spatialForestSummary,SpatialPointsLandscape-method}
\alias{spatialSoilSummary,SpatialPointsLandscape-method}
\alias{getLandscapeLayer,SpatialPointsLandscape-method}
\alias{getLandscapeVariable,SpatialPointsLandscape-method}

\title{Class \code{"SpatialPointsLandscape"}}
\description{
An S4 class that represents a set of forest stands along with their spatial location
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPointsLandscape", ...)} or by calls to function \code{\link{SpatialPointsLandscape}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class "data.frame" containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.}
    \item{\code{coords}:}{Object of class \code{"matrix"} with spatial coordinates. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string. }
    \item{\code{lct}:}{A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
    \item{\code{forestlist}:}{Object of class \code{"list"} containing a set of \code{\link{forest}} objects.  }
    \item{\code{soillist}:}{Object of class \code{"list"} containing a set of \code{\link{soil}} objects.}
    \item{\code{xlist}:}{Object of class \code{"list"} containing a set of spwb or growth model input objects.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPointsTopography}"}, directly.
Class \code{"\linkS4class{SpatialPointsDataFrame}"}, distance 2.
Class \code{"\linkS4class{SpatialPoints}"}, distance 3.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialPoints", distance 4.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialPointsLandscape", i = "ANY", j = "ANY", drop = "ANY")}: subsets the points, the corresponding topography data, and the forest and soil lists; only rows (points can be subsetted.}
    \item{head}{\code{signature(x = "SpatialPointsLandscape", n = "numeric", ... = "ANY")}: returns the first n points of object x.}
    \item{tail}{\code{signature(x = "SpatialPointsLandscape", n = "numeric", ... = "ANY")}: returns the last n points of object x.}
    \item{print}{\code{signature(x = "SpatialPointsLandscape", ...= "ANY", digits = "numeric")}: prints object x.}
    \item{show}{\code{signature(object = "SpatialPointsLandscape")}: prints object x.}
    \item{plot}{\code{signature(x = "SpatialPointsLandscape", y = "ANY")}: allows plotting ggplot maps of the landscape state.}
    \item{spplot}{\code{signature(obj = "SpatialPointsLandscape")}: allows plotting maps of the landscape state.}
    \item{spatialForestSummary}{\code{signature(object = "SpatialPointsLandscape")}: calculates a summary function for all forest stands and returns an object of class \code{\link{SpatialPointsDataFrame-class}}. }
    \item{spatialSoilSummary}{\code{signature(object = "SpatialPointsLandscape")}: calculates a summary function for the soil of all stands and returns an object of class \code{\link{SpatialPointsDataFrame-class}}. }
    \item{getLandscapeLayer}{\code{signature(obj = "SpatialGridLandscape")}: allows retrieving a variable as SpatialPixelsDataFrame.}
    \item{getLandscapeVariable}{\code{signature(obj = "SpatialGridLandscape")}: allows retrieving a variable as vector.}
 }
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{spatialForestSummary}}, \code{\link{forest}}, \code{\link{soil}}, \code{\link{getLandscapeLayer}}
}
\examples{
showClass("SpatialPointsLandscape")
}
\keyword{classes}
