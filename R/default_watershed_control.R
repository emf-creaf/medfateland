#' Default control parameters for watershed processes
#' 
#' Defines default control parameters for watershed processes
#' 
#' @param watershed_model Hydrological model for watershed processes. Only "tetis" or "serghei" are accepted.
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{watershed_model}: A string with the watershed model.}
#'    \item{\code{weather_aggregation_factor [= 1]}: An integer specifying the spatial aggregation for interpolated weather.}
#'    \item{\code{tetis_parameters}: A list of TETIS parameters with the following elements: 
#'      \itemize{
#'        \item{\code{R_localflow [= 1.0]}: Correction factor for soil hydraulic saturated conductivity (local vertical flows).}
#'        \item{\code{R_interflow [= 50.0]}: Correction factor for soil hydraulic saturated conductivity (subsurface flow between grid cells).}
#'        \item{\code{R_baseflow [= 5.0]}: Correction factor for bedrock hydraulic conductivity (groundwaterflow between grid cells).}
#'        \item{\code{n_interflow [= 1.0]}: Exponent for the determination of interflow.}
#'        \item{\code{n_baseflow [= 1.0]}: Exponent for the determination of baseflow.}
#'        \item{\code{num_daily_substeps [= 4]}: Number of daily sub-steps for interflow calculations.}
#'        \item{\code{rock_max_infiltration [= 10]}: Maximum infiltration rate (mm·day-1) for rock cells.}
#'        \item{\code{deep_aquifer_loss [= 0]}: Daily loss rate from watershed aquifer towards a deeper aquifer not connected to outlets (mm·day-1).}
#'      }
#'    }
#'    \item{\code{serghei_parameters}: A list of SERGHEI parameters with the following elements: 
#'      \itemize{
#'        \item{\code{input_dir [= ""]}: Path to SERGHEI input files.}
#'        \item{\code{output_dir [= ""]}: Path to SERGHEI output files.}
#'        \item{\code{force_equal_layer_widths [= FALSE]}: A boolean flag to force equal layer widths (taken from the first soil element) in all soils.}
#'      }
#'    }
#'  }
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#'
#' @seealso \code{\link{spwb_land}}
#' 
#' @examples default_watershed_control()
#' 
#' @export
default_watershed_control<-function(watershed_model = "tetis") {
  watershed_model <- match.arg(watershed_model, c("tetis", "serghei"))
  weather_aggregation_factor <- 1
  tetis_parameters<-list(
    R_localflow = 1.0,
    R_interflow = 50.0,
    R_baseflow = 5.0,
    n_interflow = 1.0,
    n_baseflow = 1.0,
    num_daily_substeps = 4,
    rock_max_infiltration = 10,
    deep_aquifer_loss =  0
  )
  serghei_parameters <- list(
    input_dir = "",
    output_dir = "",
    force_equal_layer_widths = FALSE
  ) 
  l <- list(watershed_model = watershed_model,
            weather_aggregation_factor = weather_aggregation_factor,
            tetis_parameters = tetis_parameters, 
            serghei_parameters = serghei_parameters)
  return(l)
}
