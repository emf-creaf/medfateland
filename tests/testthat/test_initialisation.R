test_that("Can build from forestables",{
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("forestables")
  ifn4_deadwood <- readRDS("~/OneDrive/mcaceres_work/model_development/medfate_evaluation/DeadWoodEvaluation/Rdata/IFN4_deadwood_volume_carbon.rds")
  ifn4_example <- forestables::ifn_output_example|>
    dplyr::filter(version == "ifn4")
  ifn4_example_deadwood <- ifn4_example |>
    dplyr::left_join(ifn4_deadwood, by = c("id_unique_code", "plot", "country", "version", 
                                           "class", "subclass", "province_code")) |>
    dplyr::filter(version == "ifn4")
  expect_s3_class(parse_forestable(ifn4_example[1:4,]), "sf")
  expect_s3_class(parse_forestable(ifn4_example[1:4,], keepUnfilteredCopy = TRUE), "sf")
  expect_s3_class(parse_forestable(ifn4_example_deadwood[1:4,]), "sf")
  expect_s3_class(parse_forestable(ifn4_example_deadwood[1:4,], keepUnfilteredCopy = TRUE), "sf")
})

