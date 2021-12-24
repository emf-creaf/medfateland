\name{shinyplotland}
\alias{shinyplotland}
\title{
Shiny app with interactive plots and maps
}
\description{
Creates a shiny app with interactive plots for spatial inputs and simulation results 
}
\usage{
shinyplotland(x)
}
\arguments{
  \item{x}{The object containing information to be drawn (see details).}
}
\details{
The shiny app can be used to display spatial inputs or simulation results. For spatial inputs, supply an object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}. For simulation results, supply an object of class \code{\link{summarypoints}}, \code{\link{summarypixels}} or \code{\link{summarygrid}}.
Only run this function in interactive mode. 
}
\value{
An object that represents the shiny app
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{plot.summarypoints}}
}