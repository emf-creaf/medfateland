library(medfateland)

data("example_ifn")
data("SpParamsMED")

test_that("no action checks return data frames", {
  expect_s3_class(check_topography(example_ifn, verbose = FALSE), "data.frame")
  expect_s3_class(check_land_cover(example_ifn, verbose = FALSE), "data.frame")
  expect_s3_class(check_forests(example_ifn, verbose = FALSE), "data.frame")
  expect_s3_class(check_forests(example_ifn, SpParams = SpParamsMED, verbose = FALSE), "data.frame")
  expect_s3_class(check_soils(example_ifn, verbose = FALSE), "data.frame")
})
test_that("filter action checks return sf", {
  expect_s3_class(check_topography(example_ifn, missing_action = "filter", verbose = FALSE), "sf")
  expect_s3_class(check_land_cover(example_ifn, missing_action = "filter", verbose = FALSE), "sf")
  expect_s3_class(check_forests(example_ifn, missing_action = "filter", verbose = FALSE), "sf")
  expect_s3_class(check_forests(example_ifn, SpParams = SpParamsMED, missing_action = "filter", verbose = FALSE), "sf")
})
test_that("default action checks return sf", {
  expect_s3_class(check_topography(example_ifn, missing_action = "default", verbose = FALSE), "sf")
  expect_s3_class(check_land_cover(example_ifn, missing_action = "default", verbose = FALSE), "sf")
  expect_s3_class(check_soils(example_ifn, missing_action = "default", verbose = FALSE), "sf")
})

example_wrong <- example_ifn
example_wrong$forest[[1]]$treeData$Species[2] <- "kk"
example_corrected <- check_forests(example_wrong, SpParams = SpParamsMED, missing_action = "filter")
example_wrong_2 <- example_ifn
example_wrong_2$forest[1] <- list(NULL)
example_corrected_2 <- check_forests(example_wrong_2, SpParams = SpParamsMED, default_forest = emptyforest())
test_that("filter action on wrong forests works", {
  expect_s3_class(check_forests(example_wrong, SpParams = SpParamsMED, missing_action = "filter", verbose = FALSE), "sf")
  expect_false(any(as.matrix( check_forests(example_corrected, SpParams = SpParamsMED, verbose = FALSE))))
  expect_true(any(as.matrix( check_forests(example_wrong_2, SpParams = SpParamsMED, verbose = FALSE))))
  expect_false(any(as.matrix( check_forests(example_corrected_2, SpParams = SpParamsMED, verbose = FALSE))))
})
