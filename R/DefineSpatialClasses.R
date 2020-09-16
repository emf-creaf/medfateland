setClass("SpatialPointsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list"),
         contains="SpatialPointsTopography")
setClass("SpatialGridLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list"),
         contains="SpatialGridTopography")
setClass("SpatialPixelsLandscape",
         slots=list(lct="character", forestlist="list", soillist = "list"),
         contains="SpatialPixelsTopography")
setClass("DistributedWatershed",
         slots=list(waterOrder = "numeric", waterQ = "list", queenNeigh = "list",
                    aquiferList = "list"),
         contains="SpatialPixelsLandscape")
