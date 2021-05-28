\encoding{UTF-8}
\name{plot.wswb}
\alias{plot.wswb}
\title{Displays watershed simulation results}
\description{
Produces graphical output of the results of the watershed water balance model (\code{\link{wswb}}).
}
\usage{
\method{plot}{wswb}(x, type = "Runon", summaryIndex = 1, ...)
}
\arguments{
  \item{x}{An object of class \code{wswb} with simulation results.}
  \item{type}{Type of information to be drawn (see section details).}
  \item{summaryIndex}{The index of the summary to be plotted.}
  \item{...}{Additional parameters (not used).}
}
\details{
If \code{type = "DailyRunoff"}, the function draws the simulated daily runoff (m3) exported from the watershed. The other options are the element names of \code{x$CellBalance} and \code{x$CellState} (see \code{\link{wswb}}), and the function draws a map with the desired variable corresponding to the summary indicated in \code{summaryIndex}.
}
\value{
An object of class \code{\link{ggplot}}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{wswb}}, \code{\link{spwb}}, \code{\link{DistributedWatershed-class}}
}