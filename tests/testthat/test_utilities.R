library(testthat)
context("utilities")

test_that("file_to_fluor works", {
  # Values that work
  test_values = list(
    c("430_[15373,35456].im3", "Opal 480"),
    c("431_[15373,35456].im3", "Opal 480"),
    c("480_[15373,35456].im3", "Opal 480"),
    c("520_[15322,38960].im3", "Opal 520"),
    c("520-test.im3", "Opal 520"),
    c("520notadigit.im3", "Opal 520"),
    c("DAPI_[16950,35583].im3", "DAPI"),
    c("AF_corr_20x_1.im3", "Autofluorescence"),
    c("Opal_520_corr_20x_1.im3", "Opal 520"),
    c("CD68-Opal650_1.im3", "Opal 650")
  )

  purrr::walk(test_values,
    ~expect_equal(phenoptrReports:::file_to_fluor(.x[1]), .x[2]), .info=.x[1])
})
