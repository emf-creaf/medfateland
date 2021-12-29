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
Only run this function in interactive mode. The shiny app can be used to display spatial inputs or simulation results. 

\emph{Spatial inputs}:
This is the case if the user supplies an object of class \code{\link{SpatialPointsLandscape-class}}, \code{\link{SpatialPixelsLandscape-class}} or \code{\link{SpatialGridLandscape-class}}. Allowed plots are the same as in \code{\link{getLandscapeLayer}}.

\emph{Simulation result summaries}:
This is the case if the user supplies an object of class \code{\link{summarypoints}}, \code{\link{summarypixels}} or \code{\link{summarygrid}}. Available plots depend on the summary function used to create the result summaries.

}
\value{
An object that represents the shiny app
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{plot.summarypoints}}, \code{\link{getLandscapeLayer}}
}