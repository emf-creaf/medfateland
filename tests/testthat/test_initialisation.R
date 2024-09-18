library(medfateland)
# 
# test_that("Can build from forestables",{
#   testthat::skip_on_ci()
#   testthat::skip_on_cran()
#   library(forestables)
#   ifn4_example <- ifn_output_example |>
#     dplyr::filter(version == "ifn4")
#   expect_s3_class(parse_forestable(ifn4_example[1:4,]), "sf")
#   expect_s3_class(parse_forestable(ifn4_example[1:4,], keepUnfilteredCopy = TRUE), "sf")
# })

