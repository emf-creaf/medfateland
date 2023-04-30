library(medfateland)

data("example_ifn")
data("example_watershed")
ypts = example_ifn
yws = example_watershed
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

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
#   data("exampleinterpolationdata")
#   expect_s3_class(spwb_spatial_day(ypts[1,], meteo = exampleinterpolationdata, date = date, 
#                                    SpParams = SpParamsMED, progress = FALSE), "sf")
# })
# 
# test_that("Can simulate three days over landscape using new interpolator",{
#   interpolator = with_meteo(meteoland_meteo_example) |>
#     create_meteo_interpolator(params = defaultInterpolationParams(), verbose=FALSE)
#     expect_s3_class(spwb_spatial_day(ypts[1,], meteo = interpolator, date = "2022-04-01",
#                                      SpParams = SpParamsMED, progress = FALSE), "sf")
# })