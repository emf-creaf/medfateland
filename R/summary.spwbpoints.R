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
  if(inherits(object, c("spwbgrid", "growthgrid", "fordyngrid"))) sp = "grid"
  else if(inherits(object, c("spwbpixels","growthpixels","fordynpixels"))) sp = "pixels"
  else if(inherits(object, c("spwbpoints","growthpoints","fordynpoints"))) sp = "points"
  class(res) <- c(paste0("summary", sp), "list")
  return(res)
}
summary.spwbpoints<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.spwbpixels<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.spwbgrid<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}

summary.growthpoints<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.growthpixels<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.growthgrid<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}


summary.fordynpoints<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.fordynpixels<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}
summary.fordyngrid<-function(object, summaryFunction, ...) {
  return(.summary_sp(object, summaryFunction, ...))
}