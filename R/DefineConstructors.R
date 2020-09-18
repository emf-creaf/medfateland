SpatialPointsLandscape<-function(spt, lct, forestlist, soillist) {
  #check input
  if(!inherits(spt,"SpatialPointsTopography")) 
    stop("'sgt' has to be of class 'SpatialPointsTopography'.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a list of 'soil' objects.")
  npoints = length(soillist)
  for(i in 1:npoints) {
    s = soillist[[i]]
    if(class(s) == "data.frame") {
      soillist[[i]] = soil(s)
    } else if(class(s)=="soil") {
      soillist[[i]] = s
    } else {
      stop(paste0("Wrong input soil class for",i,"\n"))
    }
  }
  xlist = vector("list", ncells)
  
  spl = new("SpatialPointsLandscape",
            lct = lct,
            forestlist = forestlist, 
            soillist = soillist,
            xlist = xlist,
            data = spt@data,
            coords.nrs = spt@coords.nrs,
            coords = spt@coords,
            bbox = spt@bbox, 
            proj4string = spt@proj4string)
  return(spl)
}

SpatialGridLandscape<-function(sgt, lct, forestlist, soillist) {
  #check input
  if(!inherits(sgt,"SpatialGridTopography")) 
    stop("'sgt' has to be of class 'SpatialGridTopography'.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a list of 'soil' objects.")
  ncells = length(soillist)
  for(i in 1:ncells) {
    s = soillist[[i]]
    if(class(s) == "data.frame") {
      soillist[[i]] = soil(s)
    } else if(class(s)=="soil") {
      soillist[[i]] = s
    } else {
      stop(paste0("Wrong input soil class for",i,"\n"))
    }
  }
  xlist = vector("list", ncells)
  sgl = new("SpatialGridLandscape",
            lct = lct,
            forestlist = forestlist, 
            soillist = soillist,
            xlist = xlist,
            data = sgt@data,
            grid =sgt@grid, 
            bbox = sgt@bbox, 
            proj4string = sgt@proj4string)
  return(sgl)
}

SpatialPixelsLandscape<-function(spxt, lct, forestlist, soillist) {
  #check input
  if(!inherits(spxt,"SpatialPixelsTopography")) 
    stop("'spxt' has to be of class 'SpatialPixelsTopography'.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a list of 'soil' objects.")
  ncells = length(soillist)
  for(i in 1:ncells) {
    s = soillist[[i]]
    if(class(s) == "data.frame") {
      soillist[[i]] = soil(s)
    } else if(class(s)=="soil") {
      soillist[[i]] = s
    } else {
      stop(paste0("Wrong input soil class for",i,"\n"))
    }
  }
  xlist = vector("list", ncells)
  spxl = new("SpatialPixelsLandscape",
             lct = lct,
             forestlist = forestlist, 
             soillist = soillist,
             xlist = xlist,
             data = spxt@data,
             coords.nrs = spxt@coords.nrs,
             grid = spxt@grid, 
             grid.index = spxt@grid.index,
             coords = spxt@coords,
             bbox = spxt@bbox, 
             proj4string = spxt@proj4string)
  return(spxl)
}

DistributedWatershed<-function(spxt, lct, forestlist, soillist) {
  #check input
  if(!inherits(spxt,"SpatialPixelsTopography")) 
    stop("'spxt' has to be of class 'SpatialPixelsTopography'.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a list of 'soil' objects.")
  
  ncells = length(soillist)
  for(i in 1:ncells) {
    s = soillist[[i]]
    if(class(s) == "data.frame") {
      soillist[[i]] = soil(s)
    } else if(class(s)=="soil") {
      soillist[[i]] = s
    } else {
      stop(paste0("Wrong input soil class for",i,"\n"))
    }
  }
  xlist = vector("list", ncells)
  aquifer = vector("list", ncells)
  
  #Take grid
  grid = spxt@grid
  coords = spxt@coords
  indices = spxt@grid.index
  elevation = spxt@data$elevation
  
  # if(verbose) cat(" - Queen neighbours")
  queenNeigh = dnearneigh(coords, 0, sqrt(prod(grid@cells.dim)))
  class(queenNeigh)<-"list"
  
  # if(verbose) cat(" - Water discharge order")
  waterOrder = order(elevation, decreasing=TRUE)
  waterQ = vector("list", length(queenNeigh))
  qfun<-function(xi, yi, zi, X, Y, Z) {
    n = length(X)
    Li = sqrt((X-xi)^2+(Y-yi)^2+(Z-zi)^2)
    dZ = zi-Z #dif. in elevation
    dZLi = dZ/Li 
    dZLi[dZ<=0] = 0 #Set to zero for neighbour cells at higher or equal elevation
    if(sum(dZLi)>0) return(dZLi/sum(dZLi))
    return(rep(0, n)) #In a flat area no discharge will be applied
  }
  for(i in 1:length(queenNeigh)) {
    wne = queenNeigh[[i]]
    waterQ[[i]] = qfun(xi = coords[i,1], yi=coords[i,2],zi = elevation[i],
                       X = coords[wne,1], Y = coords[wne,2], Z = elevation[wne])
  }  
  # if(verbose) cat(" - done.\n")

  dws = new("DistributedWatershed",
             waterOrder = waterOrder,
             waterQ = waterQ,
             queenNeigh = queenNeigh,
             aquifer = aquifer,
             lct = lct,
             forestlist = forestlist, 
             soillist = soillist,
             xlist = xlist,
             data = spxt@data,
             coords.nrs = spxt@coords.nrs,
             grid = spxt@grid, 
             grid.index = spxt@grid.index,
             coords = spxt@coords,
             bbox = spxt@bbox, 
             proj4string = spxt@proj4string)
  return(dws)
}