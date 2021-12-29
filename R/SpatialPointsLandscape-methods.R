setMethod("[", signature("SpatialPointsLandscape"),definition =
            function (x, i, j, ..., drop = TRUE) 
            {
              if (!missing(j)) 
                warning("j index ignored")
              if (is.matrix(i)) 
                stop("matrix argument not supported in SpatialPointsLandscape selection")
              if (is.character(i)) 
                i <- match(i, row.names(x))
              else if (is(i, "Spatial")) 
                i = !is.na(over(x, geometry(i)))
              if (any(is.na(i))) 
                stop("NAs not permitted in row index")
              sp = as(x,"SpatialPoints")[i, , drop=drop]
              x@coords = sp@coords
              x@bbox = sp@bbox
              x@forestlist = x@forestlist[i]
              x@soillist = x@soillist[i]
              x@xlist = x@xlist[i]
              x@lct = x@lct[i]
              x@data = x@data[i, , ..., drop = FALSE]
              x
            }
)

.print.SpatialPointsLandscape = function(x, ..., digits = getOption("digits")) {
  cat("Object of class SpatialPointsLandscape\n")
  cat(paste("Number of points:",length(x@forestlist),"\n"))
  cc = substring(paste(as.data.frame(
    t(signif(coordinates(x), digits)))),2,999)
  df = data.frame("coordinates" = cc, x@data)
  row.names(df) = row.names(x@data)
  cat(paste("Coordinates and topography:\n"))
  print(df, ..., digits = digits)
}
setMethod("print", "SpatialPointsLandscape", function(x, ..., digits = getOption("digits")) .print.SpatialPointsLandscape(x, ..., digits))

setMethod("show", "SpatialPointsLandscape", function(object) .print.SpatialPointsLandscape(object))

.head.SpatialPointsLandscape <- function(x, n=6L, ...) {
  n <- min(n, length(x))
  ix <- sign(n)*seq(abs(n))
  x[ ix , , drop=FALSE]
}
setMethod("head", "SpatialPointsLandscape", function(x, n=6L, ...) .head.SpatialPointsLandscape(x, n, ...))

.tail.SpatialPointsLandscape <- function(x, n=6L, ...) {
  n <- min(n, length(x))
  ix <- sign(n)*rev(seq(nrow(x), by=-1L, len=abs(n)))
  x[ ix , , drop=FALSE]
}
setMethod("tail", "SpatialPointsLandscape", function(x, n=6L, ...) .tail.SpatialPointsLandscape(x, n, ...))
