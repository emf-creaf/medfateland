updateState<-function(x, y) {
  if(!inherits(x, c("SpatialPointsLandscape", "SpatialPixelsLandscape", "SpatialGridLandscape"))) stop("'x' should be of class 'SpatialPointsLandscape', 'SpatialPixelsLandscape' or 'SpatialGridLandscape' ")
  if(inherits(x, "SpatiaPointsLandscape")) {
    if(!inherits(y, c("spwbpoints", "growthpoints", "fordynpoints",
                      "spwbpoints_day", "growthpoints_day")))  
      stop("'y' should be of class 'spwbpoints', 'growthpoints', 'fordynpoints', 'spwbpoints_day' or 'growthpoints_day'")
  }  
  if(inherits(x, "SpatiaPixelsLandscape")) {
    if(!inherits(y, c("spwbpixels", "growthpixels", "fordynpixels",
                      "spwbpixels_day", "growthpixels_day")))  
      stop("'y' should be of class 'spwbpixels', 'growthpixels', 'fordynpixels', 'spwbpixels_day' or 'growthpixels_day'")
  }  
  if(inherits(x, "SpatialGridLandscape")) {
    if(!inherits(y, c("spwbgrid", "growthgrid", "fordyngrid",
                      "spwbgrid_day", "growthgrid_day")))  
      stop("'y' should be of class 'spwbgrid', 'growthgrid', 'fordyngrid', 'spwbgrid_day' or 'growthgrid_day'")
  }  
  n = length(x@forestlist)
  print("xlist")
  if(!is.null(y$xlist)) {
    if(length(y$xlist)!=n) stop("'xlist' in 'y' does not have the same length as the elements in 'x'")
    for(i in 1:n) {
      x@xlist[i] = y$xlist[i]
      x@soillist[i] = y$xlist[i]$soil
    }
  }
  if(inherits(y, c("fordynpoints", "fordynpixels", "fordyngrid"))) {
    if(!is.null(y$forestlist)) {
      if(length(y$forestlist)!=n) stop("'forestlist`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        x@forestlist[i] = y$forestlist[i]
      }
    }
  }
  if(inherits(y, c("spwbland", "growthland"))) {
    print("aquifer")
    if(!is.null(y$aquifer)) {
      if(length(y$aquifer)!=n) stop("'aquifer`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        x@aquifer[i] = y$aquifer[i]
      }
    }
    print("snowpack")
    if(!is.null(y$snowpack)) {
      if(length(y$snowpack)!=n) stop("'snowpack`' in 'y' does not have the same length as the elements in 'x'")
      for(i in 1:n) {
        x@snowpack[i] = y$snowpack[i]
      }
    }
  }
  return(x)
}