#' Initialization of model inputs for spatially-distributed forest stands
#' 
#' Initializes state for local models \code{\link[medfate]{spwb}} or \code{\link[medfate]{growth}}. 
#' 
#' @param x An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{forest}: Objects of class \code{\link[medfate]{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link[medfate]{soil}} or data frames of physical properties.}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland' or 'agriculture').}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
#'     \item{\code{local_control}: A list of control parameters (optional). Used to override function parameter \code{local_control} for specific cells (values can be \code{NULL} for the remaining ones).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}}).
#' @param model A string to indicate the model, either \code{"spwb"} or \code{"growth"}.
#' @param simplify Boolean flag to simplify forest to the tree and shrub cohorts with largest leaf area index. The leaf area index of the whole tree (respectively, shrub) layer will be attributed to the selected cohort.
#'                 See function \code{\link[medfate]{forest_reduceToDominant}}. 
#' @param replace Boolean flag to replace existing initialized states
#' @param progress Boolean flag to display progress information.
#' 
#' @returns Replaces or adds column 'state' whose elements are \code{\link[medfate]{spwbInput}} or \code{\link[medfate]{growthInput}} objects 
#' and returns the modified object of class 'sf'.
#' 
#' @details
#' Initialization is dealt automatically when calling simulation functions \code{\link{spwb_spatial}},  \code{\link{growth_spatial}},
#' \code{\link{spwb_spatial_day}} or \code{\link{growth_spatial_day}}. However, function \code{initialize_landscape}  allows separating initialization from model simulations.
#' 
#' Option \code{simplify} has been implemented to allow simplification of forests to tree/shrub dominant cohorts during watershed simulations 
#' where focus is on runoff (e.g. calibration of watershed parameters or burnin periods). Elements identified as \code{result_cell} will not be simplified.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso 
#' \code{\link{spwb_spatial}}, \code{\link{spwb_spatial_day}}, 
#' \code{\link{update_landscape}}
#' 
#' @examples
#' # Load example landscape data
#' data("example_ifn")
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#' 
#' # Define local control parameters using function in medfate
#' local_control <- defaultControl()
#' 
#' # If necessary, change defaults
#' 
#' # Initialize state for 'spwb' simulations
#' example_ifn_init <- initialize_landscape(example_ifn, SpParamsMED, 
#'                                          local_control = local_control, 
#'                                          model = "spwb")
#' 
#' @name initialize_landscape
#' @export
initialize_landscape<- function(x, SpParams, local_control, model = "spwb", 
                                simplify = FALSE, 
                                replace = FALSE, progress = TRUE) {
  match.arg(model, c("spwb", "growth"))
  if(!inherits(x, "sf")) cli::cli_abort("'x' has to be an object of class 'sf'.")
  if(!("forest" %in% names(x))) cli::cli_abort("Column 'forest' must be defined.")
  if(!("soil" %in% names(x))) cli::cli_abort("Column 'soil' must be defined.")
  forestlist = x$forest
  soillist  = x$soil
  # Set local control verbose to FALSE
  local_control$verbose = FALSE
  n <- length(forestlist)
  if("state" %in% names(x)) {
    xlist  = x$state
  } else {
    xlist = vector("list",n)
  }
  if("land_cover_type" %in% names(x)) {
    landcover <- x$land_cover_type
  } else {
    landcover <- rep("wildland", n)
  }
  if("crop_factor" %in% names(x)) {
    cropfactor <- x$crop_factor
  } else {
    cropfactor <- rep(NA, n)
  }
  if("result_cell" %in% names(x)) {
    result_cell <- x$result_cell
  } else {
    result_cell <- rep(FALSE, n)
  }
  n = length(forestlist)
  if(model %in% c("spwb", "growth")) {
    init<-rep(FALSE, n)
    for(i in 1:n) {
      if(landcover[i] == "wildland") {
        f = forestlist[[i]]
        s = soillist[[i]]
        if(inherits(f, "forest") && inherits(s, c("soil", "data.frame"))) {
          init[i] = TRUE
          x_i = xlist[[i]]
          if(!replace) {
            if(inherits(x_i,"spwbInput") && model=="spwb") init[i] = FALSE
            if(inherits(x_i,"growthInput") && model=="growth") init[i] = FALSE
          }
        }
      } else if(landcover[i] == "agriculture") {
        s = soillist[[i]]
        if(inherits(s, c("soil","data.frame"))) {
          init[i] <- TRUE
          x_i = xlist[[i]]
          if(!replace) {
            if(inherits(x_i,"aspwbInput")) init[i] = FALSE
          }
        }
      }
    }
    w_init = which(init)
    if(length(w_init)>0) {
      if(progress) { 
        cli::cli_progress_step(paste0("Creating ", length(w_init), " state objects for model '", model, "'."))
        cli::cli_progress_bar(name = "Stands", total = n)
      }
      for(w in 1:length(w_init)) {
        i = w_init[w]
        s = soillist[[i]]
        local_control_i <- NULL
        if("local_control" %in% names(x)) {
          if(!is.null(x$local_control[[i]])) {
            if(inherits(x$local_control[[i]], "list")) local_control_i <- x$local_control[[i]]
          }
        }
        if(is.null(local_control_i)) local_control_i <- local_control
        if(any(is.na(s$clay))) cli::cli_abort(paste0("Missing 'clay' values in soil #", i, ". Please correct."))
        if(any(is.na(s$sand))) cli::cli_abort(paste0("Missing 'sand' values in soil #", i, ". Please correct."))
        if(any(is.na(s$bd))) cli::cli_abort(paste0("Missing 'bd' values in soil #", i, ". Please correct."))
        if(any(is.na(s$rfc))) cli::cli_abort(paste0("Missing 'rfc' values in soil #", i, ". Please correct."))
        if(inherits(s, "data.frame")) {
          s <- soil(s, VG_PTF = local_control$VG_PTF)
        }
        if(landcover[i] == "wildland") {
          f = forestlist[[i]]
          if(inherits(f, "forest") && inherits(s, "soil")) {
            if(simplify && (!result_cell[i])) {
              f = medfate::forest_reduceToDominant(f, SpParams)
            }
            if(model=="spwb") {
              xlist[[i]] = medfate::spwbInput(f, s, SpParams, local_control_i)
            } else if(model=="growth") {
              xlist[[i]] = medfate::growthInput(f, s, SpParams, local_control_i)
            }
          }
        } else if(landcover[i] == "agriculture") {
          xlist[[i]] <- medfate::aspwbInput(crop_factor = cropfactor[i], control = local_control_i, soil = s)
        }
        if(progress) cli::cli_progress_update()
      }
      if(progress) cli::cli_progress_done()
    } else {
      if(progress) cli::cli_alert_info(paste0("All state objects are already available for '", model, "'."))
    }
  } 
  x[["state"]] <- xlist
  return(x)
}