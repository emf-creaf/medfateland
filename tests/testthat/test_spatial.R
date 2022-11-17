library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
ypts = sp_to_sf(examplepointslandscape)
yws = sp_to_sf(examplewatershed)

data("examplemeteo")
data("exampleinterpolationdata")
data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwb_spatial(ypts, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial(yws, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(yws, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
})

# test_that("Can simulate three days over landscape using old interpolator",{
#   expect_s3_class(spwb_spatial(ypts[1,], meteo = exampleinterpolationdata, dates = dates, 
#                                SpParams = SpParamsMED, progress = FALSE), "sf")
# })
