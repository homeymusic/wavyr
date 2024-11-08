# tests/testthat/test_wavelength_spectrum.R

test_that("we can create a new wavelength spectrum with wavelengths and amplitudes", {
  # Create a wavelength_spectrum object
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

test_that("wavelength_spectrum can calculate fundamental_cycle_length", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length
  expect_true(is.numeric(wavelength_spectrum_obj$fundamental_cycle_length()))
})

test_that("wavelength_spectrum can calculate fractions", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- wavelength_spectrum_obj$fractions()
  expect_equal(fractions$num, c(3, 1, 3))
  expect_equal(fractions$den, c(1, 1, 2))
})
