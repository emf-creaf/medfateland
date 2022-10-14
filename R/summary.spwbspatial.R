.summary_sp<-function(object, summaryFunction, ...) {
  n = length(object$resultlist)
  summarylist = vector("list", n)
  names(summarylist) = names(object$resultlist)
  for(i in 1:n) {
    if(!is.null(object$resultlist[[i]])) {
      argList = list(object=object$resultlist[[i]],...)
      summarylist[[i]] = do.call(summaryFunction, args=argList)
    }
  }
  res = list(sp = object$sp, summarylist = summarylist)
  class(res) <- c("summaryspatial", "list")
  return(res)
}
summary.spwbspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.growthspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.fordynspatial<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}