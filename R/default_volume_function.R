#' Default volume function
#' 
#' Example function for estimating wood volume (in m3/ha) from a tree table. 
#' 
#' @details Users should define their own functions taking into account that:
#'  \itemize{
#'    \item{Input should be named 'x' and consist of a tree table with columns 'DBH' (cm), 'Height' (cm),
#'    and 'N' (ind./ha).}
#'    \item{Output should be a numeric vector of length equal to the number of tree records in 'x'}
#'  }
#' 
default_volume_function<-function(x){
  return((1/3) * pi * (x$DBH/200)^2 * (x$Height/100) * x$N) #m3/ha
}
