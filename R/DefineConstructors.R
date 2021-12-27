SpatialPointsLandscape<-function(spt, lct, forestlist, soillist) {
  #check input
  if(!inherits(spt,"SpatialPointsTopography")) 
    stop("'spt' has to be of class 'SpatialPointsTopography'.")
  id_names_spt = row.names(spt@coords)
  if(is.null(id_names_spt))
    stop("'spt' needs row names in coordinates, to be used as point IDs.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a named list of 'forest' objects.")
  id_names_fl = names(forestlist)
  if(is.null(id_names_fl)) 
    stop("'forestlist' has to be a named list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a named list of 'data.frame' or 'soil' objects.")
  id_names_sl = names(soillist)
  if(is.null(id_names_sl)) 
    stop("'soillist' has to be a named list of 'forest' objects.")
  if(!is.null(lct)) {
    if(!inherits(lct,"character")) 
      stop("'lct' has to be a named character vector.")
    id_names_lct = names(lct)
    if(is.null(id_names_lct)) 
      stop("'lct' has to be a named character vector.")
  } else {
    lct = rep("wildland", length(id_names_fl))
    names(lct) <- id_names_fl
    id_names_lct <- id_names_fl
  }
  # Merge id names
  id_names_all = unique(c(id_names_spt, id_names_lct, id_names_fl, id_names_sl))
  l_ini = length(id_names_all)
  id_names_all = id_names_all[id_names_all %in% id_names_spt]
  id_names_all = id_names_all[id_names_all %in% id_names_lct]
  id_names_all = id_names_all[id_names_all %in% id_names_fl]
  id_names_all = id_names_all[id_names_all %in% id_names_sl]
  npoints = length(id_names_all)
  if(npoints < l_ini) warning(paste0("Input names are not the same: ",npoints - lini, " point IDs will be discarded!"))
  
  # Subset (and reorder) vectors
  spt = spt[id_names_all]
  lct = lct[id_names_all]
  soillist = soillist[id_names_all]
  forestlist = forestlist[id_names_all]
  
  xlist = vector("list", npoints)
  names(xlist) = id_names_all
  
  for(i in 1:npoints) {
    f = forestlist[[i]]
    s = soillist[[i]]
    if(inherits(s,"data.frame")) {
      soillist[[i]] = soil(s)
    } else if(inherits(s,"soil")) {
      soillist[[i]] = s
    } else {
      stop(paste0("Wrong input soil class for",i,"\n"))
    }
    if(!inherits(f, "forest")) {
      stop(paste0("Wrong input forest class for",i,"\n"))
    }
  }

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

DistributedWatershed<-function(spxt, lct, forestlist, soillist, bedrock, channel, 
                               validateOutlets = TRUE) {
  #check input
  if(!inherits(spxt,"SpatialPixelsTopography")) 
    stop("'spxt' has to be of class 'SpatialPixelsTopography'.")
  if(!inherits(forestlist,"list")) 
    stop("'forestlist' has to be a list of 'forest' objects.")
  if(!inherits(soillist,"list")) 
    stop("'soillist' has to be a list of 'soil' objects.")
  if(!inherits(bedrock,"data.frame")) 
    stop("'bedrock' has to be a data frame.")
  
  cat(paste("Checking soils and forests ...\n"))
  
  ncells = length(soillist)
  for(i in 1:ncells) {
    if(lct[i] %in% c("wildland", "agriculture")) {
      f = forestlist[[i]]
      s = soillist[[i]]
      if(!is.null(s)) {
        if(class(s) == "data.frame") {
          soillist[[i]] = soil(s)
        } else if(class(s)=="soil") {
          soillist[[i]] = s
        } else {
          stop(paste0("Wrong input soil class for",i,"\n"))
        }
      }
      if(is.null(f) || is.na(f)) {
        forestlist[[i]] = emptyforest()
      }
    } else {
      forestlist[[i]] = NA #Set to missing if lct does not allow soil
      soillist[[i]] = NA #Set to missing if lct does not allow soil
    }
  }
  xlist = vector("list", ncells)
  aquifer = rep(0.0,ncells)
  snowpack = rep(0.0,ncells)
  
  cat(paste("Checking bedrock data ...\n"))
  
  #Remove NA data
  bedrock$Porosity[is.na(bedrock$Porosity)] = mean(bedrock$Porosity, na.rm=T)
  minPorosity = 0.005
  bedrock$Porosity[bedrock$Porosity<minPorosity] = minPorosity
  bedrock$Conductivity[is.na(bedrock$Conductivity)] = mean(bedrock$Conductivity, na.rm=T)
  bedrock$DepthToBedrock[is.na(bedrock$DepthToBedrock)] = mean(bedrock$DepthToBedrock, na.rm=T)
  
  #Take grid
  grid = spxt@grid
  coords = spxt@coords
  elevation = spxt@data$elevation
  slope = spxt@data$slope
  aspect = spxt@data$aspect
  
  # if(verbose) cat(" - Queen neighbours")
  neighFun<-function(coords, grid) {
    queenNeigh = dnearneigh(coords, 0, sqrt(2)*sqrt(prod(grid@cellsize)))
    attributes(queenNeigh)<-NULL
    class(queenNeigh)<-"list"
    return(queenNeigh)
  }
  waterQFun <-function(queenNeigh, coords, elevation) {
    Q = vector("list", length(queenNeigh))
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
      Q[[i]] = qfun(xi = coords[i,1], yi=coords[i,2],zi = elevation[i],
                    X = coords[wne,1], Y = coords[wne,2], Z = elevation[wne])
    }  
    return(Q)
  }
  
  # if(verbose) cat(" - Water discharge order")
  cat(paste("Calculating neighborhood ...\n"))
  queenNeigh = neighFun(coords, grid)
  numNeigh = sapply(queenNeigh,"length")
  # print(table(numNeigh))
  waterOrder = order(elevation, decreasing=TRUE)
  waterQ = waterQFun(queenNeigh, coords, elevation)
  # if(verbose) cat(" - done.\n")

  if(validateOutlets) {
    cat(paste("Removing non-valid outlet cells (may take some time) ...\n"))
    nrem = 0
    outlets = which(unlist(lapply(waterQ, sum))==0)
    validOutlets = channel[outlets]
    validOutlets[numNeigh[outlets]==8] = TRUE
    while(sum(validOutlets)<length(validOutlets)) {
      toRemove = outlets[!validOutlets]
      nrem = nrem + length(toRemove)
      waterOrder = waterOrder[-toRemove]
      queenNeigh = queenNeigh[-toRemove]
      bedrock = bedrock[-toRemove,]
      lct = lct[-toRemove]
      channel = channel[-toRemove]
      coords = coords[-toRemove,]
      elevation = elevation[-toRemove]
      slope = slope[-toRemove]
      aspect = aspect[-toRemove]
      xlist = xlist[-toRemove]
      forestlist = forestlist[-toRemove]
      soillist = soillist[-toRemove]
      aquifer = aquifer[-toRemove]
      snowpack = snowpack[-toRemove]
      spxt = meteoland::SpatialPixelsTopography(SpatialPoints(coords, spxt@proj4string),
                                                elevation = elevation, slope = slope, aspect = aspect,
                                                grid = grid)
      queenNeigh = neighFun(coords, grid)
      numNeigh = sapply(queenNeigh,"length")
      waterOrder = order(elevation, decreasing=TRUE)
      waterQ = waterQFun(queenNeigh, coords, elevation)
      outlets = which(unlist(lapply(waterQ, sum))==0)
      validOutlets = channel[outlets]
      validOutlets[numNeigh[outlets]==8] = TRUE
    } 
    cat(paste0("Removed ", nrem,  " out of ", ncells, " (",round(100*nrem/ncells, 2),"%) grid cells.\n"))
  }
  dws = new("DistributedWatershed",
             waterOrder = waterOrder,
             waterQ = waterQ,
             queenNeigh = queenNeigh,
             bedrock = bedrock,
             channel = channel,
             aquifer = aquifer,
             snowpack = snowpack,
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