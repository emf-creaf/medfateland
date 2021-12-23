\name{defaultWatershedCorrectionFactors}
\alias{defaultWatershedCorrectionFactors}
\title{
Default correction factors for watershed hydraulics
}
\description{
Defines default values for correction factors used to modulate watershed hydraulic parameters
}
\usage{
defaultWatershedCorrectionFactors()
}
\value{
A list with the following items:
\itemize{
 \item{\code{Rdrain}: Correction factor for vertical hydraulic saturated conductivity between soil and aquifer.}
 \item{\code{Rinterflow}: Correction factor for soil hydraulic saturated conductivity (subsurface flow between grid cells).}
 \item{\code{Rbaseflow}: Correction factor for bedrock hydraulic conductivity (groundwaterflow between grid cells).}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{spwbland}}
}
\examples{
defaultWatershedCorrectionFactors()
}
