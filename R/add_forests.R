
#' Add forests
#'
#' Creates and adds forest data to an \code{\link[sf]{sf}} object by reading from tree and shrub data tables
#' 
#' @param x An object of class \code{\link[sf]{sf}} with a valid CRS definition, and a column called 'id'. 
#' @param tree_table A data frame with tree records in rows and attributes in columns. Tree records can correspond to individual trees or groups of trees with an associated density.
#' @param tree_mapping  A named character vector to specify mappings of columns in \code{tree_table} into attributes of \code{treeData}. Accepted names (and the corresponding specifications for the columns in \code{tree_table}) are:
#' \itemize{
#' \item{"id": Forest stand id.}
#' \item{"Species": Species code (should follow codes in \code{SpParams}).}
#' \item{"Species.name": Species name. In this case, the species code will be drawn by matching names with species names in \code{SpParams}.}
#' \item{"N": Tree density (in ind./ha).}
#' \item{"DBH": Diameter at breast height (in cm).}
#' \item{"Height": Tree height (in cm).}
#' \item{"plot.size": Plot size (in m2) to which each record refers to. This is used to calculate tree density (stems per hectare) when not supplied.}
#' \item{"Z50": Depth (in mm) corresponding to 50 percent of fine roots.}
#' \item{"Z95": Depth (in mm) corresponding to 95 percent of fine roots.}
#' }
#' @param shrub_table A data frame with shrub records in rows and attributes in columns. Records can correspond to individual shrubs (with crown dimensions and height) or groups of shrubs with an associated cover estimate.
#' @param shrub_mapping A named character vector to specify mappings of columns in \code{shrub_table} into attributes of \code{shrubData}. Accepted names (and the corresponding specifications for the columns in \code{shrub_table}) are:
#' \itemize{
#' \item{"id": Forest stand id.}
#' \item{"Species": Species code (should follow codes in \code{SpParams}).}
#' \item{"Species.name": Species name. In this case, the species code will be drawn by matching names with species names in \code{SpParams}.}
#' \item{"Cover": Shrub cover (in percent).}
#' \item{"D1": Shrub largest crown diameter (in cm).}
#' \item{"D2": Shrub crown diameter orthogonal to the largest one (in cm).}
#' \item{"Height": Shrub height (in cm).}
#' \item{"plot.size": Plot size (in m2) to which each record refers to. This is used to calculate shrub cover when shrub data is given at the individual level.}
#' \item{"Z50": Depth (in mm) corresponding to 50 percent of fine roots.}
#' \item{"Z95": Depth (in mm) corresponding to 95 percent of fine roots.}
#' }
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}) from which valid species names are drawn.
#' @param merge_trees A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link[medfate]{forest_mergeTrees}}).
#' @param merge_shrubs A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link[medfate]{forest_mergeShrubs}}).
#' @param progress A logical flag to include a progress bar while processing the data.
#'
#' @details The current implementation will replace existing forests of the indicated 'id' values.
#'
#' @return A modified object of class \code{\link[sf]{sf}} with column 'forest'.
#' @seealso [impute_forests()], \code{\link[medfate]{forest_mapWoodyTables}}, \code{\link[medfate]{forest_mergeTrees}}
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @export
#'
#' @examples
#' 
#' # Load tree data
#' data(poblet_trees)
#' 
#' # Load species parameters
#' data(SpParamsMED)
#' 
#' # Define sf with three stands
#' cc <- rbind(c(1.0215, 41.3432),
#'             c(1.0219, 41.3443), 
#'             c(1.0219, 41.3443))
#' d <- data.frame(lon = cc[,1], lat = cc[,2], 
#'                 id = c("POBL_CTL", "POBL_THI_BEF", "POBL_THI_AFT"))
#' x <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
#' x
#' 
#' # Define tree mapping 
#' mapping <- c("id" = "Plot.Code", "Species.name" = "Species", "DBH" = "Diameter.cm")
#' 
#' # Read tree data (warnings are raised)
#' y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
#'
#' # Correct scientific name for downy oak and repeat to avoid losing tree records
#' poblet_trees$Species[poblet_trees$Species=="Quercus humilis"] <- "Quercus pubescens"
#' y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
#' 
#' # Display summary of first forest
#' summary(y_1$forest[[1]], SpParamsMED)
#' 
#' # Add sampled plot surface and repeat reading to correct tree density
#' poblet_trees$PlotSurface <- 706.86
#' mapping <- c(mapping, "plot.size" = "PlotSurface")
#' 
#' y_2 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
#' summary(y_2$forest[[1]], SpParamsMED)
#' 
#' # Check forests (height is missing!)
#' check_forests(y_2)
#' 
#' # Estimate tree height using general allometric
#' poblet_trees$Height.cm <- 100 * 1.806*poblet_trees$Diameter.cm^0.518
#' 
#' #Modify mapping to include height and repeat
#' mapping <- c(mapping, "Height" = "Height.cm")
#' 
#' y_3 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
#' summary(y_3$forest[[1]], SpParamsMED)
#' 
#' # Final check
#' check_forests(y_3)
#' 
add_forests<-function(x, 
                      tree_table = NULL, tree_mapping = NULL, 
                      shrub_table = NULL, shrub_mapping = NULL, 
                      merge_trees = TRUE, merge_shrubs = TRUE, 
                      SpParams = NULL, progress = FALSE) {
  if(!inherits(x, "sf"))  cli::cli_abort("Object 'x' has to be of class 'sf'.")
  if(!("id" %in% names(x))) cli::cli_abort("Column 'id' needs to be defined in 'x'.")
  ids<- character(0)
  if(!is.null(tree_table)) {
    if(!inherits(tree_table, "data.frame"))  cli::cli_abort("Object 'tree_table' has to be of class 'data.frame'.")
    if(is.null(tree_mapping)) stop("You need to specify a mapping for 'tree_table'")
    ids <- sort(unique(c(ids, tree_table[[tree_mapping[["id"]]]])), decreasing = FALSE)
    if("plot.size" %in% names(tree_mapping)) tree_mapping <- c(tree_mapping, "plot_size_x" = tree_mapping[["plot.size"]])
  }  
  if(!is.null(shrub_table)) {
    if(!inherits(shrub_table, "data.frame"))  cli::cli_abort("Object 'shrub_table' has to be of class 'data.frame'.")
    if(is.null(shrub_mapping)) stop("You need to specify a mapping for 'shrub_table'.")
    ids <- sort(unique(c(ids, shrub_table[[shrub_mapping[["id"]]]])), decreasing = FALSE)
    if("plot.size" %in% names(shrub_mapping)) shrub_mapping <- c(shrub_mapping, "plot_size_y" = shrub_mapping[["plot.size"]])
  }  
  if(is.null(tree_table) && is.null(shrub_table)) cli::cli_abort("Provide either 'tree_table' or 'shrub_table', and corresponding mapping.")
  
  if(progress) cli::cli_alert_info(paste0("Number of forest stands to be read: ", length(ids)))
  
  if(!("forest" %in% names(x))) {
    if(progress) cli::cli_progress_step("Defining new column 'forest'")
    x$forest <- vector("list", nrow(x))
  }
  if(progress) {
    cli::cli_progress_step("Parsing forest data")
    cli::cli_progress_bar("Locations", total = length(ids))
  }
  for(id in ids) {
    if(progress) cli::cli_progress_update()
    f_id = emptyforest()
    if(!is.null(tree_table)) {
      sel_tree <- tree_table[[tree_mapping[["id"]]]]==id
      tree_id <- tree_table[sel_tree, , drop = FALSE]
      f_id$treeData = forest_mapTreeTable(x = tree_id, mapping_x = tree_mapping,  
                                          SpParams=SpParams)
    }
    if(!is.null(shrub_table)) {
      sel_shrub <- shrub_table[[shrub_mapping[["id"]]]]==id
      shrub_id <- shrub_table[sel_shrub, , drop = FALSE]
      f_id$shrubData = forest_mapShrubTable(y = shrub_id, mapping_y = shrub_mapping,  
                                            SpParams=SpParams)
    }
    if(merge_trees)  f_id <- medfate::forest_mergeTrees(f_id)
    if(merge_shrubs)  f_id <- medfate::forest_mergeShrubs(f_id)
    rows_id <- which(x$id==id)
    if(length(rows_id)>0) x$forest[[rows_id]] <- f_id
  }
  if(progress) {
    cli::cli_progress_done()
  }
  return(sf::st_as_sf(tibble::as_tibble(x)))
}