#' Default control parameters for watershed processes
#' 
#' Defines default control parameters for watershed processes
#' 
#' @param watershed_model Either "tetis" or "serghei"
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{watershed_model}: A string with the watershed model.}
#'    \item{\code{tetis_correction_factors}: A list with the following elements: 
#'      \itemize{
#'        \item{\code{Rdrain}: Correction factor for vertical hydraulic saturated conductivity between soil and aquifer.}
#'        \item{\code{Rinterflow}: Correction factor for soil hydraulic saturated conductivity (subsurface flow between grid cells).}
#'        \item{\code{Rbaseflow}: Correction factor for bedrock hydraulic conductivity (groundwaterflow between grid cells).}
#'      }
#'    }
#'    \item{\code{serghei_input_dir}: Path to SERGHEI input files.}
#'    \item{\code{serghei_output_dir}: Path to SERGHEI output files.}
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
  tetis_correction_factors<-list(
    Rdrain = 1.0,
    Rinterflow = 1.0,
    Rbaseflow = 10.0
  )
  l <- list(watershed_model = watershed_model,
            tetis_correction_factors = tetis_correction_factors, 
            serghei_input_dir = "",
            serghei_output_dir = "")
  return(l)
}
