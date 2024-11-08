# tests/testthat/test_wavelength_spectrum.R

test_that("wavelength_spectrum object can be created with multiple wavelengths and amplitudes", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_equal(wavelength_spectrum_obj$component, c(1, 0.5, 0.33))
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("wavelength_spectrum can calculate fundamental_cycle_length", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_equal(wavelength_spectrum_obj$fundamental_cycle_length(), 2)
})
