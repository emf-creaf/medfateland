library(medfateland)

data("example_ifn")
ypts_swpb <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                 model = "spwb", progress = FALSE)
ypts_growth <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                   model = "growth", progress = FALSE)

data("examplemeteo")



data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-01"), by="day")

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwb_spatial(ypts_swpb[1:2,], meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts_growth[1:2,],, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
})


test_that("Can simulate one year forest dynamics with management",{
  expect_s3_class(fordyn_spatial(example_ifn[1,], meteo = examplemeteo, 
                               SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate 3 days over landscape using new meteoland interpolator",{
  interpolator = meteoland::with_meteo(meteoland_meteo_example, verbose = FALSE) |>
    meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)
  datesMeteo <- as.Date(stars::st_get_dimension_values(interpolator, "date"))
  expect_s3_class(spwb_spatial(ypts_swpb[1,], meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts_growth[1,], meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
})
