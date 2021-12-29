\encoding{UTF-8}
\name{plot.spwbland}
\alias{plot.spwbland}
\alias{plot.growthland}
\title{Displays watershed simulation results}
\description{
Produces graphical output of the results of a watershed simulation model (\code{\link{spwbland}} or \code{\link{growthland}}).
}
\usage{
\method{plot}{spwbland}(x, variable = "Runon", date = NULL, ...)
\method{plot}{growthland}(x, variable = "Runon", date = NULL, ...)
}
\arguments{
  \item{x}{An object of class \code{spwbland} with simulation results.}
  \item{variable}{Type of information to be drawn (see section details).}
  \item{date}{The date of the summary to be plotted. Only used if variable is not \code{"DailyRunoff"} (see details).}
  \item{...}{Additional parameters (passed to \code{\link{plot.summarypixels}}).}
}
\details{
If \code{type = "DailyRunoff"}, the function draws the simulated daily runoff (m3) exported from the watershed. The other options are the element names of cell balance or state (see \code{\link{spwbland}}), and the function will call \code{\link{plot.summarypixels}}).
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
