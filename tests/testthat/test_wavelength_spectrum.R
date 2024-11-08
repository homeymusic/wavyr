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


test_that("fundamental cycle length is calculated correctly", {

  # Create a wavelength_spectrum object
  wavelengths <- tibble::tibble(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum_obj <- wavelength_spectrum(wavelengths)

  # Calculate the fundamental cycle length
  fundamental_cycle_length <- fundamental_cycle_length(wavelength_spectrum_obj)

  # Expect the fundamental cycle length to equal the fundamental wavelength
  expect_equal(fundamental_cycle_length, max(wavelengths$wavelength))
})

test_that("fundamental wavelength is calculated correctly", {

  # Create a wavelength_spectrum object
  wavelengths <- tibble::tibble(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum_obj <- wavelength_spectrum(wavelengths)

  # Calculate the fundamental wavelength
  fundamental_wavelength <- fundamental(wavelength_spectrum_obj)

  # Expectations to check the fundamental wavelength calculation
  expect_equal(fundamental_wavelength, max(wavelengths$wavelength))
})
