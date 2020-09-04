spwbgrid<-function(y, SpParams, meteo, dates = NULL,
                  summaryFreq = "years", trackSpecies = numeric(),
                  control = defaultControl()) {

  #check input
  if(!inherits(y, "SpatialGridLandscape"))
    stop("'y' has to be of class 'SpatialGridLandscape'.")
  if(!inherits(meteo,"SpatialGridMeteorology") &&
     !inherits(meteo,"data.frame"))
    stop("'meteo' has to be of class 'SpatialGridMeteorology' or 'data.frame'.")
  if(!is.null(dates)) if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'.")

  sp = spTransform(as(y, "SpatialPoints"), CRS("+proj=longlat"))
  latitude = sp@coords[,2]
  elevation = y@data$elevation
  slope = y@data$slope
  aspect = y@data$aspect
  
  if(inherits(meteo,"data.frame")) {
    oneMeteoCell = ("MeanTemperature" %in% names(meteo))
    datesMeteo = as.Date(row.names(meteo))
  } else {
    datesMeteo = meteo@dates
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is nnot a subset of dates in 'meteo'.")
  }
  date.factor = cut(dates, breaks=summaryFreq)
  df.int = as.numeric(date.factor)
  nDays = length(dates)
  nCells = length(y@forestlist)
  nSummary = sum(table(date.factor)>0)

  #Print information area
  cat("\n------------  spwbgrid ------------\n")
  cat(paste("Grid cells: ", nCells,", area: ", areaSpatialGrid(y)/10000," ha\n", sep=""))
  cat(paste("Number of days to simulate: ",nDays,"\n", sep=""))
  cat(paste("Number of summaries: ", nSummary,"\n\n", sep=""))

  #Set control drainage to false
  control$drainage = FALSE

  if(control$verbose) cat(paste("Preparing spwb input"))
  spwbInputList = vector("list", nCells)
  patchsize = NA
  for(i in 1:nCells) {
    yid = y@forestlist[[i]]
    patchsize = yid$patchsize
    soil = y@soillist[[i]]
    if((!is.na(yid)) && (!is.na(soil))) {
      xi = forest2spwbInput(yid, soil, SpParams, control)
      spwbInputList[[i]] = xi
    }  else {
      spwbInputList[[i]] = NA
    }
  }
  cat(paste(" - number of cells with spwbInput == NA: ", sum(is.na(spwbInputList)),"\n\n", sep=""))


  #Output matrices
  Runon = matrix(0,nrow=nCells, ncol=nSummary)
  colnames(Runon) = levels(date.factor)[1:nSummary]
  Runoff = Runon
  Infiltration = Runon
  Rain =Runon
  Snow =Runon
  Interception =Runon
  DeepDrainage = Runon
  SoilEvaporation = Runon
  Transpiration = Runon
  LandscapeBalance = data.frame(Precipitation = rep(0, nSummary),
                                Snow = rep(0, nSummary),
                                Rain = rep(0, nSummary),
                                Interception = rep(0, nSummary),
                                SoilEvaporation = rep(0,nSummary),
                                Transpiration = rep(0, nSummary),
                                Runoff = rep(0, nSummary),
                                DeepDrainage= rep(0, nSummary))


  nTrackSpecies = length(trackSpecies)
  if(nTrackSpecies>0) {
    DI = array(0.0,dim=c(nCells, nSummary, nTrackSpecies))
    Transpiration = array(0.0,dim=c(nCells, nSummary, nTrackSpecies))
  } else {
    DI = NULL
    Transpiration = NULL
  }

  for(day in 1:nDays) {
    cat(paste("Day #", day))
    i = which(datesMeteo == dates[day]) #date index in meteo data
    doy = as.numeric(format(dates[day],"%j"))
    datechar = as.character(dates[day])
    if(inherits(meteo,"SpatialGridMeteorology")) {
      ml = meteo@data[[i]]
      gridMinTemperature = as.numeric(ml["MinTemperature"])
      gridMaxTemperature = as.numeric(ml["MaxTemperature"])
      gridMinRelativeHumidity = as.numeric(ml["MinRelativeHumidity"])
      gridMaxRelativeHumidity = as.numeric(ml["MaxRelativeHumidity"])
      gridPrecipitation = as.numeric(ml["Precipitation"])
      gridRadiation = as.numeric(ml["Radiation"])
      gridWindSpeed = as.numeric(ml["WindSpeed"])
    } else {
      if(!oneMeteoCell) { # Read meteo grid from file
        f = paste(meteo$dir[i], meteo$filename[i],sep="/")
        if(!file.exists(f)) stop(paste("Meteorology file '", f,"' does not exist!", sep=""))
        ml = readmeteorologygrid(f)
        gridMinTemperature = as.numeric(ml["MinTemperature"])
        gridMaxTemperature = as.numeric(ml["MaxTemperature"])
        gridMinRelativeHumidity = as.numeric(ml["MinRelativeHumidity"])
        gridMaxRelativeHumidity = as.numeric(ml["MaxRelativeHumidity"])
        gridPrecipitation = as.numeric(ml["Precipitation"])
        gridRadiation = as.numeric(ml["Radiation"])
        gridWindSpeed = as.numeric(ml["WindSpeed"])
      } else { # repeat values for all cells
        gridMinTemperature = rep(meteo[i,"MeanTemperature"], nCells)
        gridMaxTemperature = rep(meteo[i,"MaxTemperature"], nCells)
        gridMinRelativeHumidity = rep(meteo[i,"MinRelativeHumidity"], nCells)
        gridMaxRelativeHumidity = rep(meteo[i,"MaxRelativeHumidity"], nCells)
        gridPrecipitation = rep(meteo[i,"Precipitation"], nCells)
        gridRadiation = rep(meteo[i, "Radiation"], nCells)
        gridWindSpeed = rep(meteo[i, "WindSpeed"], nCells)
      }
    }
    df = .spwbgridDay(y@lct, spwbInputList, y@soillist,
                     y@waterOrder, y@queenNeigh, y@waterQ,
                     datechar,
                     gridMinTemperature, gridMaxTemperature, gridMinRelativeHumidity, gridMaxRelativeHumidity,
                     gridPrecipitation, gridRadiation, gridWindSpeed,
                     latitude, elevation, slope, aspect,
                     patchsize)
    ifactor = df.int[day]
    Runon[,ifactor] = Runon[,ifactor] + df$WaterBalance$Runon
    Runoff[,ifactor] = Runoff[,ifactor] + df$WaterBalance$Runoff
    Rain[,ifactor] = Rain[,ifactor] + df$WaterBalance$Rain
    Snow[,ifactor] = Snow[,ifactor] + df$WaterBalance$Snow
    Interception[,ifactor] = Interception[,ifactor] + (df$WaterBalance$Rain-df$WaterBalance$NetRain)
    Infiltration[,ifactor] = Infiltration[,ifactor] + df$WaterBalance$Infiltration
    DeepDrainage[,ifactor] = DeepDrainage[,ifactor] + df$WaterBalance$DeepDrainage
    SoilEvaporation[,ifactor] = SoilEvaporation[,ifactor] + df$WaterBalance$SoilEvaporation
    Transpiration[,ifactor] = Transpiration[,ifactor] + df$WaterBalance$Transpiration
    
    #Landscape balance
    LandscapeBalance$Rain[ifactor]= LandscapeBalance$Rain[ifactor] + sum(df$WaterBalance$Rain, na.rm=T)/nCells
    LandscapeBalance$Snow[ifactor]= LandscapeBalance$Snow[ifactor] + sum(df$WaterBalance$Snow, na.rm=T)/nCells
    LandscapeBalance$Interception[ifactor]= LandscapeBalance$Interception[ifactor] + (sum(df$WaterBalance$Rain, na.rm=T)-sum(df$WaterBalance$NetRain, na.rm=T))/nCells
    LandscapeBalance$DeepDrainage[ifactor] = LandscapeBalance$DeepDrainage[ifactor] + sum(df$WaterBalance$DeepDrainage, na.rm=T)/nCells
    LandscapeBalance$SoilEvaporation[ifactor] = LandscapeBalance$SoilEvaporation[ifactor] + sum(df$WaterBalance$SoilEvaporation, na.rm=T)/nCells
    LandscapeBalance$Transpiration[ifactor] = LandscapeBalance$Transpiration[ifactor] + sum(df$WaterBalance$Transpiration, na.rm=T)/nCells
    LandscapeBalance$Runoff[ifactor] = LandscapeBalance$Runoff[ifactor] + df$RunoffExport/nCells
    if(control$verbose) cat("\n")
  }
  #Average summaries
  cat("\n------------  spwbgrid ------------\n")

  LandscapeBalance$Precipitation = LandscapeBalance$Rain + LandscapeBalance$Snow
  CellBalance<-list(Rain = Rain, Snow = Snow, Interception = Interception, Runon = Runon, Runoff=Runoff,
                    Infiltration=Infiltration, DeepDrainage = DeepDrainage,
                    SoilEvaporation = SoilEvaporation, Transpiration = Transpiration)
  l <- list(grid = y@grid, LandscapeBalance = LandscapeBalance,
            CellBalance = CellBalance)
  class(l)<-c("spwbgrid","list")
  return(l)
}
