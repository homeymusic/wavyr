test_that("spectrum object can be created with component and amplitude vectors", {
  spectrum_obj <- spectrum(c(1, 0.5, 0.33), c(1.0, 0.8, 0.5))

  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("fundamental_cycle_length method works in spectrum superclass", {
  spectrum_obj <- spectrum(c(1, 0.5, 0.33), c(1.0, 0.8, 0.5))

  expect_equal(spectrum_obj$fundamental_cycle_length(), 2)
})
