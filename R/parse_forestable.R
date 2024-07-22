.parse_forest <- function(tree, understory, country, version = NA, 
                          keepSpeciesCodes = TRUE,
                          filterDeadTrees = TRUE,
                          filterCutTrees = TRUE,
                          minimumTreeDBH = 0.1) {
  is_shrub <- FALSE
  if(!is.null(understory)) {
    if((!is.null(understory$shrub)) && (length(understory$shrub)==1)) {
      is_shrub <- (nrow(understory$shrub[[1]]) > 0)
    }
  }
  is_tree <- (!is.null(tree)) && (nrow(tree)>0) 
  if(is_tree || is_shrub) {
    f <- emptyforest()
    if(is_tree) {
      f$treeData <- tree |>
        dplyr::select(.data$sp_name, .data$sp_code, .data$dbh, .data$height, .data$density_factor) |>
        dplyr::rename(Species = .data$sp_name,
                      SpeciesCode = .data$sp_code,
                      DBH = .data$dbh,
                      Height = .data$height,
                      N = .data$density_factor) |>
        dplyr::mutate(DBH = as.numeric(DBH),
                      Height = as.numeric(Height)*100,
                      N = as.numeric(N)) |>
        dplyr::mutate(Z50 = as.numeric(NA),
                     Z95 = as.numeric(NA))
      if(country == "ES") {
        dead_codes_ifn = c("888", "999")
        cut_codes_ifn = "000"
        if(version =="ifn3") {
          f$treeData$tree_ifn2 <- tree$tree_ifn2
          f$treeData$tree_ifn3 <- tree$tree_ifn3
          if(filterDeadTrees) {
            f$treeData <- f$treeData[!(f$treeData$tree_ifn3 %in% dead_codes_ifn), , drop = FALSE]
          }
          if(filterCutTrees) {
            f$treeData <- f$treeData[!(f$treeData$tree_ifn3 %in% cut_codes_ifn), , drop = FALSE]
          }
        } else if(version =="ifn4") {
          f$treeData$tree_ifn3 <- tree$tree_ifn3
          f$treeData$tree_ifn4 <- tree$tree_ifn4
          if(filterDeadTrees) {
            f$treeData <- f$treeData[!(f$treeData$tree_ifn4 %in% dead_codes_ifn), , drop = FALSE]
          }
          if(filterCutTrees) {
            f$treeData <- f$treeData[!(f$treeData$tree_ifn4 %in% cut_codes_ifn), , drop = FALSE]
          }
        }
      }
      f$treeData <- f$treeData |>
        dplyr::filter(DBH >= minimumTreeDBH)
      if(!keepSpeciesCodes) {
        f$treeData <- f$treeData |>
          dplyr::select(-.data$SpeciesCode)
      }
    }
    if(is_shrub) {
      shrub <- understory$shrub[[1]]
      f$shrubData <- shrub |>
        dplyr::select(.data$sp_name, .data$sp_code, .data$height, cover) |>
        dplyr::rename(Species = .data$sp_name,
                      SpeciesCode = .data$sp_code,
                      Height = .data$height,
                      Cover = .data$cover) |>
        dplyr::mutate(Height = as.numeric(Height),
                      Cover = as.numeric(Cover),
                      Z50 = as.numeric(NA),
                      Z95 = as.numeric(NA))
      
      if(!keepSpeciesCodes) {
        f$shrubData <- f$shrubData |>
          dplyr::select(-.data$SpeciesCode)
      }
      
    }
    return(f)
  }
  return(NULL)
}

#' Parse forestable
#' 
#' Transforms a data frame or sf object issued from package forestables into an sf object
#' for simulations with medfateland.
#'
#' @param x A data frame or sf object issued from package forestables.
#' @param keepSpeciesCodes Keeps forest inventory species codes.
#' @param filterDeadTrees If TRUE, filters out dead trees (Spanish forest inventory IFN3 or IFN4).
#' @param filterCutTrees If TRUE, filters out cut trees (Spanish forest inventory IFN3 or IFN4).
#' @param minimumTreeDBH Minimum DBH for keeping a tree record.
#' @param progress A logical flag to include a progress bar while processing the data.
#'
#' @details
#' This function retrieves the following information from the forestables object:
#' \itemize{
#'   \item{Id unique code, survey year, non-unique plot code and country.}
#'   \item{Plot location. Output geometry is always points in WGS 84. Note that exact coordinates are not normally given in forest inventory data.}
#'   \item{Elevation, slope and aspect, whenever available}
#'   \item{Tree and understory data. The function will create a column \code{forest} with this information.}
#' }
#' 
#' @return An sf object including a 'forest' column
#' @export
#'
parse_forestable <- function(x, 
                             keepSpeciesCodes = TRUE,
                             filterDeadTrees = TRUE, 
                             filterCutTrees = TRUE,
                             minimumTreeDBH = 0.1,
                             progress = FALSE) {
  
  forestables_vars <- c("id_unique_code", "year", "plot", "country", "tree", "understory")
  if("version" %in% names(x)) forestables_vars <- c(forestables_vars, "version")
  if("elev" %in% names(x)) forestables_vars <- c(forestables_vars, "elev")
  if("slope" %in% names(x)) forestables_vars <- c(forestables_vars, "slope")
  if("aspect" %in% names(x)) forestables_vars <- c(forestables_vars, "aspect")
  if(!inherits(x, "sf")) { # Taken from forestables (to avoid importing the package)
    x <- x[,c(forestables_vars, "coordx", "coordy", "crs", "coord_sys")]
    x <- x |>
      dplyr::group_by(.data$crs) |>
      dplyr::group_modify(
        .f = \(crs_subset, crs) {
          sf::st_as_sf(crs_subset, coords = c("coordx", "coordy"), crs = unique(crs$crs)) |>
            sf::st_transform(crs = 4326) |>
            dplyr::mutate(crs_orig = crs$crs) |>
            dplyr::rename(coord_sys_orig = "coord_sys")
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(crs = 4326) |>
      sf::st_as_sf()
  }
  x <- x[, forestables_vars]
  if(!("version" %in% names(x))) x[["version"]] <- NA 
  x[["forest"]] <- vector("list", nrow(x)) 
  if(progress) cli::cli_progress_bar("Parsing plots", total = nrow(x))
  for(i in 1:nrow(x)) {
    f <- .parse_forest(x$tree[[i]], x$understory[[i]], x$country[[i]], 
                       version = x$version[[i]],
                       keepSpeciesCodes = keepSpeciesCodes,
                       filterDeadTrees = filterDeadTrees,
                       filterCutTrees = filterCutTrees,
                       minimumTreeDBH = minimumTreeDBH)
    if(!is.null(f)) x$forest[[i]] <- f
    if(progress) cli::cli_progress_update()
  }
  if(progress) cli::cli_progress_done()
  
  x <- x |>
    dplyr::rename(id = id_unique_code) |>
    dplyr::select(-tree, -understory) |>
    dplyr::relocate(forest, .before = geometry) |>
    dplyr::relocate(geometry, .after = id)
  if("elev" %in% names(x)) x <- x |> dplyr::rename(elevation = elev)
  if("version" %in% names(x)) x <- x |> dplyr::relocate(version, .after = country)
  return(x)
}