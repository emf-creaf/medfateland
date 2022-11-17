library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
ypts = sp_to_sf(examplepointslandscape)
yws = sp_to_sf(examplewatershed)

data("examplemeteo")
data("SpParamsMED")
date = "2001-03-01"

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwb_spatial_day(ypts, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial_day(yws, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(yws, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
})
# 
# test_that("Can simulate one day over landscape using old interpolator",{
#   expect_s3_class(spwb_spatial_day(ypts[1,], meteo = exampleinterpolationdata, date = date, 
#                                    SpParams = SpParamsMED, progress = FALSE), "sf")
# })