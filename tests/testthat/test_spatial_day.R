library(medfateland)

data("example_ifn")
ypts_swpb <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                  model = "spwb", progress = FALSE)
ypts_growth <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                    model = "growth", progress = FALSE)

data("examplemeteo")



data("SpParamsMED")
date = "2001-03-01"

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwb_spatial_day(ypts_swpb[1:2,], meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts_growth[1:2,], meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate one day over landscape using new interpolator",{
  interpolator = with_meteo(meteoland_meteo_example, verbose = FALSE) |>
    create_meteo_interpolator(params = defaultInterpolationParams(), verbose=FALSE)
  expect_s3_class(spwb_spatial_day(ypts_swpb[1:2,], meteo = interpolator, date = "2022-04-01",
                                     SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts_growth[1:2,], meteo = interpolator, date = "2022-04-01",
                                   SpParams = SpParamsMED, progress = FALSE), "sf")
})