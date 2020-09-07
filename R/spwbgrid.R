spwbgrid<-function(y, SpParams, meteo, dates = NULL,
                  summaryFreq = "years",
                  control = defaultControl()) {

  #check input
  if(!inherits(y, "SpatialGridLandscape"))
    stop("'y' has to be of class 'SpatialGridLandscape'.")
  if(!inherits(meteo,"SpatialGridMeteorology") &&
     !inherits(meteo,"data.frame") &&
     !inherits(meteo, "MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'SpatialGridMeteorology', 'MeteorologyInterpolationData' or 'data.frame'.")
  if(!is.null(dates)) if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'.")

  sp = spTransform(as(y, "SpatialPoints"), CRS("+proj=longlat"))
  latitude = sp@coords[,2]
  elevation = y@data$elevation
  slope = y@data$slope
  aspect = y@data$aspect

  if(inherits(meteo,"data.frame")) {
    oneMeteoCell = ("MeanTemperature" %in% names(meteo))
    datesMeteo = as.Date(row.names(meteo))
  } else if(inherits(meteo, "MeteorologyInterpolationData")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialGridMeteorology")) {
    datesMeteo = meteo@dates
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  date.factor = cut(dates, breaks=summaryFreq)
  df.int = as.numeric(date.factor)
  nDays = length(dates)
  nCells = length(y@forestlist)
  nSummary = sum(table(date.factor)>0)
  t.df = as.numeric(table(df.int))

  #Determine outlet cells (those without downhill neighbors)
  outlets = which(unlist(lapply(y@waterQ, sum))==0)

  patchsize = y@forestlist[[1]]$patchsize

  #Print information area
  cat("\n------------  spwbgrid ------------\n")
  cat(paste("Grid cells: ", nCells,", patchsize: ", patchsize," m2, area: ", nCells*patchsize/10000," ha\n", sep=""))
  cat(paste("Meteorological input class: ", class(meteo),"\n", sep=""))
  cat(paste("Number of days to simulate: ",nDays,"\n", sep=""))
  cat(paste("Number of landscape summaries: ", nSummary,"\n", sep=""))
  cat(paste("Number of outlet cells: ", length(outlets),"\n\n"))

  #Set control drainage to false
  control$drainage = FALSE

  cat(paste("Preparing spwb input"))
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
  DailyRunoff = matrix(0,nrow = nDays, ncol = length(outlets))
  colnames(DailyRunoff) = outlets
  rownames(DailyRunoff) = as.character(dates)
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
  SWE = Runon
  Psi1 = Runon
  WTD = Runon
  Volume = Runon
  LandscapeBalance = data.frame(Precipitation = rep(0, nSummary),
                                Snow = rep(0, nSummary),
                                Rain = rep(0, nSummary),
                                Interception = rep(0, nSummary),
                                SoilEvaporation = rep(0,nSummary),
                                Transpiration = rep(0, nSummary),
                                Runoff = rep(0, nSummary),
                                DeepDrainage= rep(0, nSummary))


  summary_function = function(object, model="SX") {
    list(SWE = object$SWE,
         Psi1 = soil_psi(object)[1],
         Volume = sum(soil_water(object, model)),
         WTD = soil_waterTableDepth(object))
    }

  cat(paste("Running simulations:\n"))
  for(day in 1:nDays) {
    # cat(paste("Day #", day))
    cat(".")
    i = which(datesMeteo == dates[day]) #date index in meteo data
    doy = as.numeric(format(dates[day],"%j"))
    datechar = as.character(dates[day])
    if(inherits(meteo,"MeteorologyInterpolationData")) {
      ml = interpolationgrid(meteo, y, dates[day], verbose = TRUE)
      ml = ml@data
      # print(class(ml))
      # stop("kk")
      gridMinTemperature = ml$MinTemperature
      gridMaxTemperature = ml$MaxTemperature
      gridMinRelativeHumidity = ml$MinRelativeHumidity
      gridMaxRelativeHumidity = ml$MaxRelativeHumidity
      gridPrecipitation = ml$Precipitation
      gridRadiation = ml$Radiation
      gridWindSpeed = ml$WindSpeed
    } else if(inherits(meteo,"SpatialGridMeteorology")) {
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

    summary_day = spatialSoilSummary(y, summary_function, control$soilFunctions)
    summary_df = summary_day@data
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
    SWE[,ifactor] = SWE[,ifactor] + summary_df$SWE/t.df[ifactor]
    Psi1[,ifactor] = Psi1[,ifactor] + summary_df$Psi1/t.df[ifactor]
    Volume[,ifactor] = Volume[,ifactor] + summary_df$Volume/t.df[ifactor]
    WTD[,ifactor] = WTD[,ifactor] + summary_df$WTD/t.df[ifactor]

    DailyRunoff[day,] = df$WaterBalance$Runoff[outlets]*patchsize/1e6 ## Runoff in m3/day

    #Landscape balance
    LandscapeBalance$Rain[ifactor]= LandscapeBalance$Rain[ifactor] + sum(df$WaterBalance$Rain, na.rm=T)/nCells
    LandscapeBalance$Snow[ifactor]= LandscapeBalance$Snow[ifactor] + sum(df$WaterBalance$Snow, na.rm=T)/nCells
    LandscapeBalance$Interception[ifactor]= LandscapeBalance$Interception[ifactor] + (sum(df$WaterBalance$Rain, na.rm=T)-sum(df$WaterBalance$NetRain, na.rm=T))/nCells
    LandscapeBalance$DeepDrainage[ifactor] = LandscapeBalance$DeepDrainage[ifactor] + sum(df$WaterBalance$DeepDrainage, na.rm=T)/nCells
    LandscapeBalance$SoilEvaporation[ifactor] = LandscapeBalance$SoilEvaporation[ifactor] + sum(df$WaterBalance$SoilEvaporation, na.rm=T)/nCells
    LandscapeBalance$Transpiration[ifactor] = LandscapeBalance$Transpiration[ifactor] + sum(df$WaterBalance$Transpiration, na.rm=T)/nCells
    LandscapeBalance$Runoff[ifactor] = LandscapeBalance$Runoff[ifactor] + df$RunoffExport/nCells
    # if(control$verbose) cat("\n")
  }
  cat("done\n\n")
  #Average summaries
  cat("\n------------  spwbgrid ------------\n")

  LandscapeBalance$Precipitation = LandscapeBalance$Rain + LandscapeBalance$Snow
  CellBalance<-list(Rain = Rain, Snow = Snow, Interception = Interception, Runon = Runon, Runoff=Runoff,
                    Infiltration=Infiltration, DeepDrainage = DeepDrainage,
                    SoilEvaporation = SoilEvaporation, Transpiration = Transpiration)
  CellState<-list(SWE = SWE, Psi1 = Psi1, Volume = Volume, WTD = WTD)
  l <- list(grid = y@grid, LandscapeBalance = LandscapeBalance,
            CellBalance = CellBalance,
            CellState = CellState,
            DailyRunoff = DailyRunoff)
  class(l)<-c("spwbgrid","list")
  return(l)
}
