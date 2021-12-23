\encoding{UTF-8}
\name{plot.spwbland}
\alias{plot.spwbland}
\alias{plot.growthland}
\title{Displays watershed simulation results}
\description{
Produces graphical output of the results of a watershed simulation model (\code{\link{spwbland}} or \code{\link{growthland}}).
}
\usage{
\method{plot}{spwbland}(x, type = "Runon", summaryIndex = 1, ...)
\method{plot}{growthland}(x, type = "Runon", summaryIndex = 1, ...)
}
\arguments{
  \item{x}{An object of class \code{spwbland} with simulation results.}
  \item{type}{Type of information to be drawn (see section details).}
  \item{summaryIndex}{The index of the summary to be plotted.}
  \item{...}{Additional parameters (not used).}
}
\details{
If \code{type = "DailyRunoff"}, the function draws the simulated daily runoff (m3) exported from the watershed. The other options are the element names of \code{x$CellBalance} and \code{x$CellState} (see \code{\link{spwbland}}), and the function draws a map with the desired variable corresponding to the summary indicated in \code{summaryIndex}.
}
\value{
An object of class \code{\link{ggplot}}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbland}}, \code{\link{DistributedWatershed-class}}
}
