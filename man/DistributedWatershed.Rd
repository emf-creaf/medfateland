\encoding{UTF-8}
\name{DistributedWatershed}
\alias{DistributedWatershed}
\title{
Defines a distributed watershed
}
\description{
Function to initialize an object of \code{\link{DistributedWatershed-class}}.
}
\usage{
DistributedWatershed(spxt, lct, forestlist, soillist, bedrock, channel, 
                     validateOutlets = TRUE)
}
\arguments{
  \item{spxt}{An object of class \code{\link{SpatialPixelsTopography}}.}
  \item{lct}{A character vector with the land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
  \item{forestlist}{A list of objects of class 'forest' with the same number of elements as spatial pixels.}
  \item{soillist}{A list of objects of class 'forest' with the same number of elements as spatial pixels.}
  \item{bedrock}{A data frame with the same number of rows as spatial pixels and containing hydraulic bedrock properties: 'DepthToBedrock' (mm), 'Conductivity' (m/day), 'Porosity' ([0-1]).}
  \item{channel}{A logical vector to indicate which watershed cells belong to the channel network.}
  \item{validateOutlets}{A flag to remove outlet cells that are outside channel network and on the boundaries of the watershed.}
}
\value{
An object of class \code{\link{DistributedWatershed-class}}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{\code{\link{SpatialPixelsLandscape}}}
\examples{
\dontrun{
code = "20054998"
spt = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/SpatialPixelsTopography/",code,".rds"))
lct = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/LandUse/",code,".rds"))
forestlist = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/Forests/",code,".rds"))
soillist = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/Soils/",code,".rds"))
bedrock = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/Bedrock/",code,".rds"))
channel = readRDS(paste0("D:/Datasets/Hydrology/Products/Catalonia/Channels/",code,".rds"))

dw = DistributedWatershed(spt, lct, forestlist, soillist, bedrock, channel)
}
}