\encoding{UTF-8}
\name{plot.summarypoints}
\alias{plot.summarypoints}
\alias{plot.summarypixels}
\alias{plot.summarygrid}
\title{Displays spatial simulation summaries}
\description{
Produces graphical output of the summaries of a simulation models
}
\usage{
\method{plot}{summarypoints}(x, variable, date, ...)
\method{plot}{summarypixels}(x, variable, date, ...)
\method{plot}{summarygrid}(x, variable, date, ...)
}
\arguments{
  \item{x}{An object of class \code{summarypoints}, \code{summarypixels} or \code{summarygrid}, depending on the function, with simulation summaries. }
  \item{variable}{The variable to be drawn.}
  \item{date}{The date of the summary to be plotted.}
  \item{...}{Additional parameters (not used).}
}
\details{
Appropriate values for \code{x} can originate from calls to summary functions (e.g. \code{\link{summary.spwbpoints}}). Alternatively, if summary functions were specified at the time of performing simulations, the result of the simulation function will already contain the summaries. The result of spatial simulation functions (e.g. \code{\link{spwbpoints}}) can be used directly thanks to the class definition of simulation results (e.g. \code{\link{spwbpoints}} returns an object of class \code{c("spwbpoints", "summarypoints", "list")}).
}
\value{
An object of class \code{\link{ggplot}}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbpoints}}, \code{\link{summary.spwbpoints}}
}
