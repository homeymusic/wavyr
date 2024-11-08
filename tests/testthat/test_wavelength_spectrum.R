# tests/testthat/test_wavelength_spectrum.R

test_that("we can create a new wavelength spectrum with separate wavelength and amplitude vectors", {
  # Create a wavelength_spectrum object with separate vectors
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check wavelength_spectrum creation
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
  expect_equal(wavelength_spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("we can create a new wavelength spectrum with a list containing wavelength and amplitude", {
  # Create a wavelength_spectrum object with a list input
  wavelength_spectrum_obj <- wavelength_spectrum(
    list(wavelength = c(1, 0.5, 0.33), amplitude = c(1.0, 0.8, 0.5))
  )

  # Expectations to check wavelength_spectrum creation
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
  expect_equal(wavelength_spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("wavelength_spectrum handles mismatched input lengths gracefully", {
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0.5), amplitude = c(1.0, 0.8, 0.5)),
    "must be the same length"
  )
})

test_that("wavelength_spectrum validates numeric input", {
  expect_error(
    wavelength_spectrum(wavelength = c("a", "b"), amplitude = c(1.0, 0.8)),
    "must be numeric"
  )
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0.5), amplitude = c("x", "y")),
    "must be numeric"
  )
})

test_that("wavelength_spectrum calculates fundamental_cycle_length correctly", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length
  expect_true(is.numeric(wavelength_spectrum_obj$fundamental_cycle_length()))
  expect_gt(wavelength_spectrum_obj$fundamental_cycle_length(), 0)
})

test_that("wavelength_spectrum calculates fractions accurately", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- wavelength_spectrum_obj$fractions()
  expect_equal(fractions$num, c(3, 1, 3))
  expect_equal(fractions$den, c(1, 1, 2))
})

test_that("wavelength_spectrum fundamental_cycle_length for single component returns 1", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1),
    amplitude = c(1.0)
  )

  # Expect the fundamental cycle length for a single component to be 1
  expect_equal(wavelength_spectrum_obj$fundamental_cycle_length(), 1)
})

test_that("wavelength_spectrum edge cases: zero or negative wavelengths", {
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0), amplitude = c(1.0, 0.5)),
    "All component values must be positive."
  )
  expect_error(
    wavelength_spectrum(wavelength = c(-1, 0.5), amplitude = c(1.0, 0.8)),
    "All component values must be positive."
  )
})
