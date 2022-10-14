\encoding{UTF-8}
\name{summary.spwbspatial}
\alias{summary.spwbspatial}
\alias{summary.growthspatial}
\alias{summary.fordynspatial}
\title{Summarizes spatial simulation results}
\description{
Creates spatial objects containing summaries of simulations
}
\usage{
\method{summary}{spwbspatial}(object, summaryFunction, ...)
\method{summary}{growthspatial}(object, summaryFunction, ...)
\method{summary}{fordynspatial}(object, summaryFunction, ...)
}
\arguments{
  \item{object}{An object of the appropriate class, containing simulation results. }
  \item{summaryFunction}{The summary function to be executed on simulation results (see details).}
  \item{...}{Additional parameters to the summary function.}
}
\details{
The function supplied should take as input an object of local simulation function, i.e. \code{\link{spwb}}, \code{\link{growth}}, or \code{\link{fordyn}}. The output should be a matrix with dates as rows and variables in columns. An example of suitable function is \code{\link{summary.spwb}}.
}
\value{
An object of class \code{\link{summaryspatial}}, with the following two elements:
\itemize{
\item{\code{sp}: An object with spatial information (of \code{SpatialPoints-class}, \code{SpatialPixels-class} or \code{SpatialGrid-class}).}
\item{\code{summarylist}: A list of model output summaries for each simulated stand.}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbspatial}}, \code{\link{plot.summaryspatial}}
}
