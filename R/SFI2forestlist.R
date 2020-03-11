SFI2forestlist<-function(SFItreeData, SFIshrubData, SpParams, SFIherbData=NULL,  control = defaultControl()) {
  
  if(sum(c("H","DBH","Species","ID") %in% names(SFItreeData))<4) stop("Columns in SFItreeData must include 'ID','Species','DBH' and 'H'")
  if(sum(c("H","FCC","Species","ID") %in% names(SFIshrubData))<4) stop("Columns in SFItreeData must include 'ID','Species','FCC' and 'H'")
  IDs = as.character(sort(unique(c(SFItreeData$ID, SFIshrubData$ID))))

  if(control$verbose) cat(paste0("Number of plots: ", length(IDs),"\n"))  
  x = SFItreeData[SFItreeData$ID %in% IDs, ]
  y = SFIshrubData[SFIshrubData$ID %in% IDs, ]
  
  toRemX = is.na(x$Species) | is.na(x$DBH) | is.na(x$H)
  if(sum(toRemX)>0) {
    if(control$verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))  
    x = x[!toRemX,]
  }
  toRemY = is.na(y$Species) | is.na(y$FCC) | is.na(y$H)
  if(sum(toRemY)>0) {
    if(control$verbose) cat(paste0("Filtered records in shrub data: ", sum(toRemY),"\n"))  
    x = x[!toRemY,]
  }
  
  if(control$verbose) cat("Translating species codes...\n")  
  
  
  SFIcodes = SpParams$IFNcodes
  x$SpeciesMF = forest_translateSFISpeciesCodes(x$Species, SFIcodes)
  y$SpeciesMF = forest_translateSFISpeciesCodes(y$Species, SFIcodes)
  #Remove NA species
  if(sum(is.na(x$SpeciesMF))>0) {
    cat(paste0("Tree data records with unrecognized SFI codes: ",sum(is.na(x$SpeciesMF)),"/", length(x$SpeciesMF),"\n"))
    if(control$verbose) {
      print(table(x$Species[is.na(x$SpeciesMF)]))
    }
    x = x[!is.na(x$SpeciesMF),]
  }
  if(sum(is.na(y$SpeciesMF))>0) {
    cat(paste0("Shrub data records with unrecognized SFI codes:",sum(is.na(y$SpeciesMF)),"/", length(y$SpeciesMF),"\n"))
    if(control$verbose) {
      print(table(y$Species[is.na(y$SpeciesMF)]))
    }
    y = y[!is.na(y$SpeciesMF),]
  }
  x$Species =x$SpeciesMF
  y$Species =y$SpeciesMF
  
  if(control$verbose) cat("Extracting SFI data...\n")  
  lx = split(x, factor(x$ID, levels=IDs))
  ly = split(y, factor(y$ID, levels=IDs))
  forestlist = Map(function(x,y, id) {
    forest_extractSFIData(x,y, id, SFIherbData = SFIherbData, SpParams=SpParams,setDefaults = TRUE)
  }, lx, ly, IDs)  
  if(control$verbose) cat("done.\n")  
  return(forestlist)
}