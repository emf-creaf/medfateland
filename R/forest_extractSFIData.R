forest_extractSFIData<-function(SFItreeData, SFIshrubData, ID, SpParams, 
                           SFIherbData = NULL, SFIcodes=NULL, 
                           patchsize = 10000, setDefaults=TRUE) {
  f = list()
  xid = SFItreeData[SFItreeData$ID==ID,]
  yid = SFIshrubData[SFIshrubData$ID==ID,]
  if(!is.null(SFIcodes)) {
    xid$SpeciesMF = forest_translateSFISpeciesCodes(xid, SFIcodes)
    yid$SpeciesMF = forest_translateSFISpeciesCodes(yid, SFIcodes)
    #Remove NA species
    if(sum(is.na(xid$SpeciesMF))>0) {
      cat(paste0("Tree data records with unrecognized SFI codes: ",sum(is.na(xid$SpeciesMF)),"/", length(xid$SpeciesMF),"\n"))
      print(table(xid$Species[is.na(xid$SpeciesMF)]))
      xid = xid[!is.na(xid$SpeciesMF),]
    }
    if(sum(is.na(yid$SpeciesMF))>0) {
      cat(paste0("Shrub data records with unrecognized SFI codes:",sum(is.na(yid$SpeciesMF)),"/", length(yid$SpeciesMF),"\n"))
      print(table(yid$Species[is.na(yid$SpeciesMF)]))
      yid = yid[!is.na(yid$SpeciesMF),]
    }
    xid$Species =xid$SpeciesMF
    yid$Species =yid$SpeciesMF
  }
    
  f$ID = ID
  f$patchsize = patchsize
  f$treeData = data.frame(Species = xid$Species, N = round(xid$N), DBH = xid$DBH, Height = xid$H*100)
  f$treeData$Z50 = rep(NA, nrow(f$treeData))
  f$treeData$Z95 = rep(NA, nrow(f$treeData))
  f$shrubData = data.frame(Species = yid$Species, Cover = as.numeric(yid$FCC), Height = yid$H*100)
  f$shrubData$Z50 =rep(NA, nrow(f$shrubData))
  f$shrubData$Z95 =rep(NA, nrow(f$shrubData))
  if(setDefaults) {
    f$treeData$Z95 = SpParams$Zmax[f$treeData$Species+1]
    f$treeData$Z95[is.na(f$treeData$Z95)] = 3000
    f$treeData$Z50 = f$treeData$Z95/4
    f$shrubData$Z95 = SpParams$Zmax[f$shrubData$Species+1]
    f$shrubData$Z95[is.na(f$shrubData$Z95)] = 1000
    f$shrubData$Z50 = f$shrubData$Z95/4
  }
  
  f$herbCover = NA
  f$herbHeight = NA
  if(!is.null(SFIherbData)) {
    if(ID %in% SFIherbData$ID) {
      f$herbCover = SFIherbData$Cover[SFIherbData$ID==ID]
      f$herbHeight = SFIherbData$Height[SFIherbData$ID==ID]
      if(length(f$herbCover)>1) f$herbCover = mean(f$herbCover, na.rm=TRUE)
      if(length(f$herbHeight)>1) f$herbHeight = mean(f$herbHeight, na.rm=TRUE)
    }
  } else {
  }
  
  if(setDefaults) {
    usp = sort(unique(c(f$treeData$Species, f$saplingData$Species, f$shrubData$Species)))
    f$seedBank = data.frame(Species = usp, Abundance = rep(100, length(usp)))
  } else {
    f$seedBank = data.frame(Species = numeric(0), Abundance = numeric(0))
  }

  class(f)<-c("forest","list")
  return(f)
}