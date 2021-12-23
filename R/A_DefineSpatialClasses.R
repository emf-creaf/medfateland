setClass("SpatialPointsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialPointsTopography")
setClass("SpatialGridLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialGridTopography")
setClass("SpatialPixelsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list", xlist = "list"),
         contains="SpatialPixelsTopography")
setClass("DistributedWatershed",
         slots=list(waterOrder = "numeric", waterQ = "list", queenNeigh = "list",
                    channel = "logical", bedrock = "data.frame", snowpack = "numeric", aquifer = "numeric"),
         contains="SpatialPixelsLandscape")
