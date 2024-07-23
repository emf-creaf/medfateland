#' Default volume function
#' 
#' Example function for estimating wood volume (in m3/ha) from a tree table or forest object. 
#' 
#' @param x A data frame with columns 'DBH', 'Height' and 'N' or a \code{\link[medfate]{forest}} object
#' @param SpParams A data frame with species parameters (not used in the default function but will be called)
#' 
#' @details Users should define their own functions taking into account that:
#'  \itemize{
#'    \item{Input should be named 'x' and consist of a tree table with tree records as rows
#'          and columns 'DBH' (cm), 'Height' (cm), and 'N' (ind./ha).}
#'    \item{Output should be a numeric vector of length equal to the number of tree records in 'x'}
#'  }
#' 
#' @returns A function amenable for wood volume estimation.
#' @export
default_volume_function<-function(x, SpParams = NULL){
  if(inherits(x, "forest")) x <- x$treeData
  vol <- (1/3) * pi * (x$DBH/200)^2 * (x$Height/100) * x$N
  vol[x$DBH < 7.5] = 0
  return(vol) #m3/ha
}
