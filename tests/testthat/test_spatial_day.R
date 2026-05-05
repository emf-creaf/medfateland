library(medfateland)
library(meteoland)

data("example_ifn")
ypts_swpb <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                  model = "spwb", progress = FALSE)
ypts_growth <- initialize_landscape(example_ifn[1:2,], SpParams = SpParamsMED, local_control = defaultControl(),
                                    model = "growth", progress = FALSE)

data("examplemeteo")


data("SpParamsMED")
date = "2001-03-01"


ypts_swpb_meteo <- ypts_swpb[1:2,]
ypts_swpb_meteo$meteo <- list(examplemeteo, examplemeteo)

ypts_growth_meteo <- ypts_growth[1:2,]
ypts_growth_meteo$meteo <- list(examplemeteo, examplemeteo)


test_that("Can simulate one day over landscape",{
  expect_s3_class(spwb_spatial_day(ypts_swpb[1:2,], meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial_day(ypts_swpb_meteo, meteo = NULL, date = date, 
                                   SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts_growth[1:2,], meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts_growth_meteo, meteo = NULL, date = date, 
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


test_that("Can simulate several consecutive days", {
  res <- NULL
  cond_vec <- logical(100)
  gdd <- 0
  for(i in 1:100) {
    date <- as.character(examplemeteo$dates[i])
    if(i == 1) {
      y <- ypts_swpb[1,, drop = FALSE]
    } else {
      y <- update_landscape(y, res)
    }
    res <- spwb_spatial_day(y, meteo = examplemeteo, date = date, 
                            SpParams = SpParamsMED, progress = FALSE)
    gdd_prev <- gdd
    gdd <- mean(res$state[[1]]$internalPhenology$gdd, na.rm = TRUE)
    cond_vec[i] = (round(gdd_prev,3) <= round(gdd,3))
    # cat(paste0(i, " : ", round(gdd_prev,3), " -> ", round(gdd, 3), "\n"))
  }
  expect_all_true(cond_vec)
})
