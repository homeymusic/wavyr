# tests/testthat/test_waveform.R

test_that("we can create a new waveform with a frequency spectrum, wavelength spectrum, and phase", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Define a phase for the waveform
  phase <- pi / 4  # An arbitrary phase

  # Create the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = phase
  )

  # Expectations to check waveform creation
  expect_s3_class(waveform_obj, "waveform")
  expect_equal(waveform_obj$phase, phase)
  expect_equal(waveform_obj$frequency_spectrum$component, c(100, 200, 300))
  expect_equal(waveform_obj$frequency_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_equal(waveform_obj$wavelength_spectrum$component, c(1, 0.5, 0.33))
  expect_equal(waveform_obj$wavelength_spectrum$amplitude, c(1.0, 0.8, 0.5))
})

test_that("we can create a general waveform with only a frequency spectrum and no wavelength spectrum or phase", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a waveform with only frequency spectrum
  waveform_obj <- waveform(frequency_spectrum = frequency_spectrum_obj)

  # Expectations to check waveform creation
  expect_s3_class(waveform_obj, "waveform")
  expect_equal(waveform_obj$frequency_spectrum$component, c(100, 200, 300))
  expect_equal(waveform_obj$frequency_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_null(waveform_obj$wavelength_spectrum)
  expect_null(waveform_obj$phase)
})

test_that("waveform with wavelength spectrum and frequency spectrum but no phase works as expected", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the waveform object without specifying phase
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Expectations to check waveform creation
  expect_s3_class(waveform_obj, "waveform")
  expect_equal(waveform_obj$frequency_spectrum$component, c(100, 200, 300))
  expect_equal(waveform_obj$frequency_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_equal(waveform_obj$wavelength_spectrum$component, c(1, 0.5, 0.33))
  expect_equal(waveform_obj$wavelength_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_null(waveform_obj$phase)
})
