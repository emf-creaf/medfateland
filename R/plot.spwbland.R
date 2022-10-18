#' Displays watershed simulation results
#' 
#' Produces graphical output of the results of a watershed simulation model (\code{\link{spwbland}} or \code{\link{growthland}}).
#' 
#' @param x An object of class \code{spwbland} with simulation results.
#' @param variable Type of information to be drawn (see section details).
#' @param date The date of the summary to be plotted. Only used if variable is not \code{"DailyRunoff"} (see details).
#' @param ... Additional parameters (passed to \code{\link{plot.summarypixels}}).
#' 
#' @details If \code{type = "DailyRunoff"}, the function draws the simulated daily runoff (m3) exported from the watershed. 
#' The other options are the element names of cell balance or state (see \code{\link{spwbland}}), 
#' and the function will call \code{\link{plot.summarypixels}}).
#' 
#' @return An object of class \code{\link{ggplot}}.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwbland}}, \code{\link{DistributedWatershed-class}}
#' 
#' @name plot.spwbland
plot.spwbland<-function(x, variable = "Runon", date = NULL, ...) {
  if(variable=="DailyRunoff") {
    df = data.frame(Date = as.Date(row.names(x$DailyRunoff)), 
                    Runoff = rowSums(x$DailyRunoff, na.rm=T))
    g1<-ggplot(df)+
      geom_line( aes_string(x="Date", y="Runoff"), col="blue")+
      ylab("Watershed runoff (m3/day)")+theme_bw()
  } else if (variable %in% c(colnames(x$summarylist[[1]]))) {
    g1 = plot.summaryspatial(x, variable, date, ...)
  } else {
    stop("Unknown plot variable")
  }
  return(g1)
}
#' @rdname plot.spwbland
plot.growthland<-function(x, variable = "Runon", date = NULL, ...) {
  return(plot.spwbland(x = x, variable = variable, date = date, ...))
}
