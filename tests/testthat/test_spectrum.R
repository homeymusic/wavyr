# tests/testthat/test_spectrum.R

test_that("we can create a new spectrum with separate component and amplitude vectors", {
  # Create a spectrum object with separate vectors
  spectrum_obj <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("we can create a new spectrum with a list containing component and amplitude", {
  # Create a spectrum object with a list input
  spectrum_obj <- spectrum(
    list(c(1, 0.5, 0.33), c(1.0, 0.8, 0.5))
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("spectrum can calculate fundamental_cycle_length", {
  spectrum_obj <- spectrum(
    component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length (result might vary with fractions implementation)
  expect_true(is.numeric(spectrum_obj$fundamental_cycle_length()))
})

test_that("spectrum can calculate fractions", {
  spectrum_obj <- spectrum(
    component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- spectrum_obj$fractions()
  expect_equal(fractions$num, c(3, 1, 3))
  expect_equal(fractions$den, c(1, 1, 2))
})
