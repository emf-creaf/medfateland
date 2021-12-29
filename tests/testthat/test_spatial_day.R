library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")
date = "2001-03-01"

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwbpoints_day(examplepointslandscape, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "spwbpoints_day")
  expect_s3_class(spwbpixels_day(examplewatershed, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "spwbpixels_day")
  expect_s3_class(growthpoints_day(examplepointslandscape, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "growthpoints_day")
  expect_s3_class(growthpixels_day(examplewatershed, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "growthpixels_day")
})
