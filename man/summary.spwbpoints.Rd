\encoding{UTF-8}
\name{summary.spwbpoints}
\alias{summary.spwbpoints}
\alias{summary.spwbpixels}
\alias{summary.spwbgrid}
\alias{summary.growthpoints}
\alias{summary.growthpixels}
\alias{summary.growthgrid}
\alias{summary.fordynpoints}
\alias{summary.fordynpixels}
\alias{summary.fordyngrid}
\title{Summarizes spatial simulation results}
\description{
Creates spatial objects containing summaries of simulations
}
\usage{
\method{summary}{spwbpoints}(object, summaryFunction, ...)
\method{summary}{spwbpixels}(object, summaryFunction, ...)
\method{summary}{spwbgrid}(object, summaryFunction, ...)
\method{summary}{growthpoints}(object, summaryFunction, ...)
\method{summary}{growthpixels}(object, summaryFunction, ...)
\method{summary}{growthgrid}(object, summaryFunction, ...)
\method{summary}{fordynpoints}(object, summaryFunction, ...)
\method{summary}{fordynpixels}(object, summaryFunction, ...)
\method{summary}{fordyngrid}(object, summaryFunction, ...)
}
\arguments{
  \item{x}{An object of the appropriate class, containing simulation results. }
  \item{summaryFunction}{The summary function to be executed on simulation results (see details).}
  \item{...}{Additional parameters to the summary function.}
}
\details{
The function supplied should take as input an object of local simulation function, i.e. \code{\link{spwb}}, \code{\link{growth}}, or \code{\link{fordyn}}. The output should be a matrix with dates as rows and variables in columns. An example of suitable function is \code{\link{summary.spwb}}.
}
\value{
An object of class \code{\link{summarypoints}}, \code{\link{summarypixels}} or \code{\link{summarygrid}}, depending on the function, with the following two elements:
\itemize{
\item{\code{sp}: An object with spatial information (of \code{SpatialPoints-class}, \code{SpatialPixels-class} or \code{SpatialGrid-class}).}
\item{\code{summarylist}: A list of model output summaries for each simulated stand.}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbpoints}}, \code{\link{plot.summarypoints}}
}
