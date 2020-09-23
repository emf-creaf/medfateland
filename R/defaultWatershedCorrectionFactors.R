defaultWatershedCorrectionFactors<-function() {
  l<-list(
    Rdrain = 1.0,
    Rinterflow = 1.0,
    Rbaseflow = 10.0
  )
  return(l)
}
