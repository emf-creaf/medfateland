.getWatershedWaterBalancePlotTypes <- function(){
  return(c("Hydrograph & Hietograph" = "Hydrograph_Hietograph",
           "PET & Precipitation" = "PET_Precipitation",
           "Channel routing" = "Channel",
           "Water exported" = "Export", 
           "Overland runoff" = "Overland_Runoff",
           "Aquifer balance" = "Aquifer_Balance",
           "Evapotranspiration" = "Evapotranspiration"))
}
.plot_watershed_wb <-function(x, type,  
                              dates = NULL, 
                              xlim = NULL, ylim=NULL, xlab=NULL, ylab=NULL, 
                              summary.freq = NULL, ...) {
  WaterBalance <- x$watershed_balance
  
  type <- match.arg(type,.getWatershedWaterBalancePlotTypes())
  df <- data.frame(row.names=as.character(WaterBalance$dates))
  df[["Date"]] = as.Date(WaterBalance$dates)
  if(type=="Hydrograph_Hietograph") {

    df[["Precipitation"]] = WaterBalance$Precipitation
    df[["WatershedDischarge"]] = rowSums(x$outlet_export_m3s)
    
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      Precipitation = tapply(df$Precipitation,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      WatershedDischarge = tapply(df$WatershedDischarge,INDEX=date.factor, FUN=mean, na.rm=TRUE))
    }
    factor <- max(df$Precipitation)/max(df$WatershedDischarge)
    maxRange <- 1.1*(max(df$Precipitation/factor) + max(df$WatershedDischarge))
    precip_labels <- function(x) {round(x*factor)}
    if(is.null(ylab)) ylab = expression(paste("Discharge (",m^{3}%.%s^{-1}, ")")) 
    g <- ggplot(df)+
      geom_tile(aes(x = .data$Date,
                    y = -1*((.data$Precipitation/factor)/2-maxRange), # y = the center point of each bar
                    height = .data$Precipitation/factor,
                    width = 1), fill = "blue", color="blue")+
      geom_line(aes(.data$Date, .data$WatershedDischarge), color = "black")+
      scale_y_continuous(name = ylab,
                         sec.axis = sec_axis(transform = ~-1*(.-maxRange),
                                             name = "Precipitation (mm)",
                                             labels = precip_labels))+
      theme_bw()
    return(g)
  } else if(type=="Channel") {
    outlet_cells <- colnames(x$outlet_export_m3s)
    outlet_channel <- as.character(unique(x$sf$target_outlet[as.numeric(colnames(x$channel_export_m3s))]))
    outlet_non_channel <- !(outlet_cells %in% outlet_channel)
    df[["WatershedDischarge"]] = rowSums(x$outlet_export_m3s)
    df[["ChannelLoad"]] = rowSums(x$channel_export_m3s)
    df[["ChannelDischarge"]] = rowSums(x$outlet_export_m3s[, !outlet_non_channel, drop = FALSE])
    df[["DirectDischarge"]] = rowSums(x$outlet_export_m3s[, outlet_non_channel, drop = FALSE])
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      WatershedDischarge = tapply(df$WatershedDischarge,INDEX=date.factor, FUN=mean, na.rm=TRUE),
                      ChannelLoad = tapply(df$ChannelLoad,INDEX=date.factor, FUN=mean, na.rm=TRUE),
                      ChannelDischarge = tapply(df$ChannelDischarge,INDEX=date.factor, FUN=mean, na.rm=TRUE),
                      DirectDischarge = tapply(df$DirectDischarge,INDEX=date.factor, FUN=mean, na.rm=TRUE))
    }
    if(is.null(ylab)) ylab = expression(m^{3}%.%s^{-1}) 
    g <- ggplot(df)+
      geom_line(aes(.data$Date, .data$ChannelLoad, color = "Channel load")) +
      geom_line(aes(.data$Date, .data$ChannelDischarge, color = "Channel discharge")) +
      geom_line(aes(.data$Date, .data$DirectDischarge, color = "Direct discharge")) +
      geom_line(aes(.data$Date, .data$WatershedDischarge, color = "Watershed discharge")) +
      scale_color_manual(name="", 
                         values=c("Watershed discharge"="black",
                                  "Channel load" = "red",
                                  "Channel discharge"="darkgreen", 
                                  "Direct discharge"="yellow"), 
                         limits = c("Watershed discharge", 
                                    "Channel load",
                                    "Channel discharge",
                                    "Direct discharge"))+
      ylab(ylab)+
      theme_bw()
    return(g)    
  } else if(type=="PET_Precipitation") {
    if(is.null(ylab)) ylab = expression(L%.%m^{-2}) 
    # For back-compatibility
    if("PET" %in% names(WaterBalance)) df[["PET"]] = WaterBalance$PET
    else df[["PET"]] =as.numeric(NA)
    df[["Precipitation"]] = WaterBalance$Precipitation
    df[["Snow"]] = WaterBalance$Snow
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      Precipitation = tapply(df$Precipitation,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      Snow = tapply(df$Snow,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      PET = tapply(df$PET,INDEX=date.factor, FUN=sum, na.rm=TRUE))
    }
    g<-ggplot(df)+
      geom_bar(aes(x=.data$Date, y=.data$Precipitation, fill="Precipitation"), stat = "identity")+
      geom_bar(aes(x=.data$Date, y=.data$Snow, fill="Snow"), stat = "identity")+
      geom_path(aes(x=.data$Date, y=.data$PET, col="PET"))+
      scale_fill_manual(name="", values=c("Precipitation"="black", "Snow"="red"))+
      scale_color_manual(name="", values=c("PET"="gray"))+
      ylab(ylab)+ xlab(xlab)+
      theme_bw()
    return(g)
  } else if(type=="Evapotranspiration") {
    if(is.null(ylab)) ylab = expression(L%.%m^{-2})
    df[["Evapotranspiration"]] = WaterBalance$Transpiration + WaterBalance$Interception + WaterBalance$HerbTranspiration + WaterBalance$SoilEvaporation
    df[["Interception"]] = WaterBalance$Interception
    df[["Transpiration"]] = WaterBalance$Transpiration
    df[["HerbTranspiration"]] = WaterBalance$HerbTranspiration
    df[["SoilEvaporation"]] = WaterBalance$SoilEvaporation
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      Evapotranspiration = tapply(df[["Evapotranspiration"]],INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      Interception = tapply(df[["Interception"]],INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      Transpiration = tapply(df[["Transpiration"]],INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      HerbTranspiration = tapply(df[["HerbTranspiration"]],INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      SoilEvaporation = tapply(df[["SoilEvaporation"]],INDEX=date.factor, FUN=sum, na.rm=TRUE))
    }
    g<-ggplot(df)+
      geom_line(aes(x=.data$Date, y=.data$Evapotranspiration, col="Total evapotranspiration"))+
      geom_line(aes(x=.data$Date, y=.data$Interception, col="Interception evaporation"))+
      geom_line(aes(x=.data$Date, y=.data$Transpiration, col="Woody transpiration"))+
      geom_line(aes(x=.data$Date, y=.data$HerbTranspiration, col="Herbaceous transpiration"))+
      geom_line(aes(x=.data$Date, y=.data$SoilEvaporation, col="Bare soil evaporation"))+
      scale_color_manual(name="", values=c("Total evapotranspiration"="black", 
                                           "Interception evaporation" = "yellow", "Woody transpiration" = "darkgreen",
                                           "Herbaceous transpiration" = "red", "Bare soil evaporation" = "blue"))+
      ylab(ylab)+ xlab(xlab)+
      theme_bw()
    return(g)
  } else if(type=="Export") {
    if(is.null(ylab)) ylab =  expression(L%.%m^{-2})    
    df[["ChannelExport"]] = WaterBalance$ChannelExport 
    df[["WatershedExport"]] = WaterBalance$WatershedExport 
    df[["DeepAquiferLoss"]] = WaterBalance$DeepAquiferLoss
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      ChannelExport = tapply(df$ChannelExport,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      WatershedExport = tapply(df$WatershedExport,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      DeepAquiferLoss = tapply(df$DeepAquiferLoss,INDEX=date.factor, FUN=sum, na.rm=TRUE))
    }
    g<-ggplot(df)+
      geom_line(aes(x=.data$Date, y=.data$WatershedExport, col="Watershed surface export"))+
      geom_line(aes(x=.data$Date, y=.data$ChannelExport, col="Channel export"))+
      geom_line(aes(x=.data$Date, y=.data$DeepAquiferLoss, col="Deep aquifer export"))+
      scale_color_manual(name="", values=c("Watershed surface export"="black", 
                                           "Channel export" = "darkgreen",
                                           "Deep aquifer export" = "darkgray"), 
                         limits = c("Watershed surface export", 
                                    "Channel export",
                                    "Deep aquifer export"))+
      ylab(ylab)+ xlab(xlab)+
      theme_bw()
    return(g)
  } else if(type=="Overland_Runoff") {
    if(is.null(ylab)) ylab =  expression(L%.%m^{-2})    
    df[["CellRunoff"]] = WaterBalance$CellRunoff 
    df[["InfiltrationExcess"]] = WaterBalance$InfiltrationExcess
    df[["SaturationExcess"]] = WaterBalance$SaturationExcess
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      InfiltrationExcess = tapply(df$InfiltrationExcess,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      SaturationExcess = tapply(df$SaturationExcess,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      CellRunoff = tapply(df$CellRunoff,INDEX=date.factor, FUN=sum, na.rm=TRUE))
    }
    g<-ggplot(df)+
      geom_line(aes(x=.data$Date, y=.data$InfiltrationExcess, col="Infiltration excess"))+
      geom_line(aes(x=.data$Date, y=.data$SaturationExcess, col="Saturation excess"))+
      geom_line(aes(x=.data$Date, y=.data$CellRunoff, col="Overland runoff"))+
      scale_color_manual(name="", values=c("Infiltration excess" = "red", "Saturation excess" = "blue",
                                           "Overland runoff" = "black"),
                         limits =c("Overland runoff", "Infiltration excess","Saturation excess")) +
      ylab(ylab)+ xlab(xlab)+
      theme_bw()
    return(g)
  } else if(type=="Aquifer_Balance") {
    if(is.null(ylab)) ylab =  expression(L%.%m^{-2})    
    df[["CapillarityRise"]] = WaterBalance$CapillarityRise
    df[["DeepDrainage"]] = WaterBalance$DeepDrainage
    df[["AquiferExfiltration"]] = WaterBalance$AquiferExfiltration
    df[["NegativeAquiferCorrection"]] = WaterBalance$NegativeAquiferCorrection
    if(!is.null(dates)) df = df[df$Date %in% dates,]
    if(!is.null(summary.freq)) {
      date.factor = cut(as.Date(df$Date), breaks=summary.freq)
      df = data.frame(Date = as.Date(as.character(levels(date.factor))),
                      CapillarityRise = tapply(df$CapillarityRise,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      DeepDrainage = tapply(df$DeepDrainage,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      AquiferExfiltration = tapply(df$AquiferExfiltration,INDEX=date.factor, FUN=sum, na.rm=TRUE),
                      NegativeAquiferCorrection = tapply(df$NegativeAquiferCorrection,INDEX=date.factor, FUN=sum, na.rm=TRUE))
    }
    g<-ggplot(df)+
      geom_line(aes(x=.data$Date, y=.data$CapillarityRise, col="Capillarity rise"))+
      geom_line(aes(x=.data$Date, y=.data$DeepDrainage, col="Deep drainage"))+
      geom_line(aes(x=.data$Date, y=.data$AquiferExfiltration, col="Aquifer exfiltration"))+
      geom_line(aes(x=.data$Date, y=.data$NegativeAquiferCorrection, col="Neg. aquifer corr."))+
      scale_color_manual(name="", values=c("Capillarity rise" = "red", "Deep drainage" = "blue", "Aquifer exfiltration" = "black",
                                           "Neg. aquifer corr." = "darkgray"))+
      ylab(ylab)+ xlab(xlab)+
      theme_bw()
    return(g)
  } 
}

#' Displays watershed-level simulation results
#'
#' Plots time series of the watershed-level balance results of simulations with \code{spwb_land}, \code{growth_land} or \code{fordyn_land}.
#' 
#' @param x An object of class \code{spwb_land}, \code{growth_land} or \code{fordyn_land}.
#' @param type The information to be plotted (see details).
#' @param dates A Date vector with a subset of dates to be plotted.
#' @param summary.freq Frequency of summary statistics (see \code{\link{cut.Date}}).
#' @param ... Additional parameters for function \code{plot} (not used).
#'
#' @details The following plots are currently available:
#' \itemize{
#'   \item{\code{"Hydrograph_Hietograph"}: A combination of hydrograph and hietograph (in a secondary, reversed, axis).}
#'   \item{\code{"PET_Precipitation"}: Potential evapotranspiration, rainfall and snow.}
#'   \item{\code{"Channel"}: Partitioning of overall discharge into discharge from channel and direct outlet discharge. Channel loading is also shown to evidence the routing effect.}
#'   \item{\code{"Export"}: Water exported through different fluxes: (total watershed export, channel routing, deep drainage).}
#'   \item{\code{"Overland_Runoff"}: Origin of overland runoff flows (i.e. saturation excess or infiltration excess).}
#'   \item{\code{"Aquifer_Balance"}: Water exchanged between soil and aquifer (i.e. soil deep drainage and capillarity rise).}
#'   \item{\code{"Evapotranspiration"}: Interception, woody transpiration, herb transpiration and soil evaporation.}
#' }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @returns A ggplot object
#' 
#' @seealso  \code{\link{spwb_land}}, \code{\link{growth_land}}, \code{\link{fordyn_land}}, 
#'           \code{\link{plot_summary}}, \code{\link{shinyplot.spwb_land}}
#' @export
#'
#' @name plot.spwb_land
plot.spwb_land <- function(x, type="Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...) {
  .plot_watershed_wb(x, type = type, dates = dates, summary.freq = summary.freq,...)
}
#' @export
#' @rdname plot.spwb_land
plot.growth_land <- function(x, type="Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...) {
  .plot_watershed_wb(x, type = type, dates = dates, summary.freq = summary.freq, ...)
}
#' @export
#' @rdname plot.spwb_land
plot.fordyn_land <- function(x, type="Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...) {
  .plot_watershed_wb(x, type = type, dates = dates, summary.freq = summary.freq,...)
}