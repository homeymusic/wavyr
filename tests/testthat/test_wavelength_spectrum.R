# tests/testthat/test_wavelength_spectrum.R

test_that("we can create a new wavelength spectrum with multiple wavelengths and amplitudes", {

  # Create a wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check wavelength_spectrum creation
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_equal(length(wavelength_spectrum_obj$wavelength), 3)
  expect_equal(wavelength_spectrum_obj$wavelength, c(1, 0.5, 0.33))
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})
