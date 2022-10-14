\encoding{UTF-8}
\name{plot.summaryspatial}
\alias{plot.summaryspatial}
\title{Displays spatial simulation summaries}
\description{
Produces graphical output of the summaries of a simulation models
}
\usage{
\method{plot}{summaryspatial}(x, variable, date, ...)
}
\arguments{
  \item{x}{An object of class \code{summaryspatial}, with simulation summaries. }
  \item{variable}{The variable to be drawn.}
  \item{date}{The date of the summary to be plotted.}
  \item{...}{Additional parameters (passed to scale definition, such as \code{limits}).}
}
\details{
Appropriate values for \code{x} can originate from calls to summary functions (e.g. \code{\link{summary.spwbspatial}}). Alternatively, if summary functions were specified at the time of performing simulations, the result of the simulation function (e.g. \code{\link{spwbspatial}}) will already contain the summaries.
}
\value{
An object of class \code{\link{ggplot}}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\seealso{
\code{\link{spwbspatial}}, \code{\link{summary.spwbspatial}}
}
