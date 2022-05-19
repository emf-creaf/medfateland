library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwbpoints(examplepointslandscape, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "spwbpoints")
  expect_s3_class(spwbpixels(examplewatershed, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "spwbpixels")
  expect_s3_class(growthpoints(examplepointslandscape, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "growthpoints")
  expect_s3_class(growthpixels(examplewatershed, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "growthpixels")
})
