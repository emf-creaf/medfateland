library(medfateland)

data("examplewatershed")
data("examplemeteo")
data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwbland(examplewatershed, meteo = examplemeteo, dates = dates, summaryFreq = "month", 
                           SpParams = SpParamsMED, progress = FALSE), "spwbland")
  expect_s3_class(growthland(examplewatershed, meteo = examplemeteo, dates = dates, summaryFreq = "month", 
                             SpParams = SpParamsMED, progress = FALSE), "growthland")
})
