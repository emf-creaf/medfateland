SFI2forestlist<-function(SFItreeData, SFIshrubData, SpParams, SFIherbData=NULL,  control = defaultControl()) {
  IDs = as.character(sort(unique(c(SFItreeData$ID, SFIshrubData$ID))))

  if(control$verbose) cat(paste0("Number of plots: ", length(IDs),"\n"))  
  x = SFItreeData[SFItreeData$ID %in% IDs, ]
  y = SFIshrubData[SFIshrubData$ID %in% IDs, ]
  
  toRemX = is.na(x$Species) | is.na(x$DBH) | is.na(x$H)
  if(sum(toRemX)>0) {
    if(control$verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))  
    x = x[!toRemX,]
  }
  toRemY = is.na(y$Species) | is.na(y$FCC) | is.na(x$H)
  if(sum(toRemX)>0) {
    if(control$verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))  
    x = x[!toRemX,]
  }
  
  if(control$verbose) cat("Translating species codes...")  
  
  
  SFIcodes = SpParams$IFNcodes
  x$Species = forest_translateSFISpeciesCodes(x$Species, SFIcodes)
  y$Species = forest_translateSFISpeciesCodes(y$Species, SFIcodes)

  if(control$verbose) cat("Extracting SFI data...")  
  lx = split(x, factor(x$ID, levels=IDs))
  ly = split(y, factor(y$ID, levels=IDs))
  forestlist = Map(function(x,y, id) {
    forest_extractSFIData(x,y, id, SFIherbData = SFIherbData, SpParams=SpParams,setDefaults = TRUE)
  }, lx, ly, IDs)  
  if(control$verbose) cat("done.\n")  
  return(forestlist)
}