#' Build SpatialPixelsTopography for watersheds
#'
#' Defines objects \code{\link{SpatialPixelsTopography-class}} for watersheds whose boundaries 
#' are specified in a \code{\link{SpatialPolygons-class}} object.
#' 
#' @param boundaries An object \code{\link{SpatialPolygons-class}} or \code{\link{SpatialPolygonsDataFrame-class}} containing the boundaries of the watershed.
#' @param grid An object \code{\link{GridTopology-class}} containing the grid definition.
#' @param topo An object \code{\link{SpatialGridTopography-class}} containing the topographic features at appropriate resolution
#' @param proj4string Projection string (i.e. \code{\link{CRS}}) corresponding to the grid definition.
#' @param merge A boolean flag to indicate wether polygons should be merged.
#' 
#' @returns A list of objects \code{\link{SpatialPixelsTopography-class}}, each one describing the topography of the watershed. If \code{merge = TRUE} or the number of polygons is one, a single \code{\link{SpatialPixelsTopography-class}} object.
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{SpatialPixelsTopography}}
buildWatershedTopography<-function(boundaries, grid, topo, proj4string, merge = FALSE) {
  b = as(boundaries, "SpatialPolygons")
  cat(paste("Number of watersheds: ", length(b),"\n"))
  cat(paste("1. Building grid coordinates.\n"))
  sp_fullgrid=SpatialPoints(coordinates(grid), proj4string)
  cat(paste("2. Intersecting with boundaries.\n"))
  overboundaries=over(spTransform(sp_fullgrid, b@proj4string), b)
  cat(paste("3. Building SpatialPixelsTopography object(s).\n"))
  if(!merge) {
    l = vector("list", length(b))
    for(i in 1:length(b)) {
      sel = (overboundaries==i)
      sel[is.na(sel)] = FALSE
      sp_cells = sp_fullgrid[sel]
      sp2 = spTransform(sp_cells, topo@proj4string)
      gi = getGridIndex(sp2@coords, topo@grid)
      topo_df = topo@data[gi,]
      spdf_topo=SpatialPixelsTopography(sp_cells, topo_df$elevation, topo_df$slope, topo_df$aspect)
      l[[i]] = spdf_topo
    }
    if(length(l)==1) l = l[[1]]
  } else {
    sel = !is.na(overboundaries)
    sp_cells = sp_fullgrid[sel]
    sp2 = spTransform(sp_cells, topo@proj4string)
    gi = getGridIndex(sp2@coords, topo@grid)
    topo_df = topo@data[gi,]
    l=SpatialPixelsTopography(sp_cells, topo_df$elevation, topo_df$slope, topo_df$aspect)
  }
  return(l)
}