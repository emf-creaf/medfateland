library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")
date = "2001-03-01"

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwbspatial_day(examplepointslandscape, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "spwbspatial_day")
  expect_s3_class(spwbspatial_day(examplewatershed, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "spwbspatial_day")
  expect_s3_class(growthspatial_day(examplepointslandscape, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "growthspatial_day")
  expect_s3_class(growthspatial_day(examplewatershed, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "growthspatial_day")
})
