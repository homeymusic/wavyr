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

test_that("wavelength_spectrum can combine with another wavelength_spectrum within tolerance", {
  # Create two wavelength_spectrum objects
  wavelength_spectrum1 <- wavelength_spectrum(
    wavelength = c(2.0, 1.0, 0.67),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum2 <- wavelength_spectrum(
    wavelength = c(2.0, 1.01, 0.67),  # Close values to test tolerance
    amplitude = c(0.5, 0.4, 0.3)
  )

  # Combine wavelength_spectrum1 and wavelength_spectrum2 with a tolerance of 0.05
  combined_wavelength_spectrum <- combine_spectra(
    wavelength_spectrum1,
    wavelength_spectrum2,
    tolerance = 0.05
  )

  # Expected combined wavelength and amplitude values
  expected_wavelengths <- c(2.0, 1.0, 0.67)
  expected_amplitudes <- c(1.5, 1.2, 0.8)

  # Test the combined wavelength_spectrum
  expect_s3_class(combined_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(combined_wavelength_spectrum$component %>% sort(),
               expected_wavelengths %>% sort(),
               tolerance = 0.1)
  expect_equal(combined_wavelength_spectrum$amplitude %>% sort(),
               expected_amplitudes %>% sort(),
               tolerance = 0.1)
})

# tests/testthat/test_wavelength_spectrum.R

test_that("wavelength_spectrum has accessible wavelength field", {
  # Create a wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(2.0, 1.0, 0.67),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Check that `wavelength` field is accessible and correct
  expect_equal(wavelength_spectrum_obj$wavelength, c(2.0, 1.0, 0.67))
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
})
