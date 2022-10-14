library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwbspatial(examplepointslandscape, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "spwbspatial")
  expect_s3_class(spwbspatial(examplewatershed, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "spwbspatial")
  expect_s3_class(growthspatial(examplepointslandscape, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "growthspatial")
  expect_s3_class(growthspatial(examplewatershed, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "growthspatial")
})
