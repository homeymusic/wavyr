# tests/testthat/test_waveform.R

test_that("we can create a new waveform based on a frequency spectrum, wavelength spectrum, and phase", {

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

  # Create the waveform object with all parameters
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

test_that("waveform can be created with a frequency spectrum and automatically generates wavelength spectrum and beat wavelengths", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the waveform object without specifying wavelength_spectrum or phase
  waveform_obj <- waveform(frequency_spectrum = frequency_spectrum_obj, speed_of_sound = 343)

  # Expectations to check waveform creation
  expect_s3_class(waveform_obj, "waveform")

  # Check that wavelength components match expected values
  expected_wavelengths <- 343 / c(100, 200, 300)
  expect_equal(waveform_obj$wavelength_spectrum$component[1:3], expected_wavelengths)

  # Check that beat wavelengths are correctly added
  beat_wavelengths <- abs(outer(expected_wavelengths, expected_wavelengths, "-"))
  beat_wavelengths <- beat_wavelengths[lower.tri(beat_wavelengths)]
  expect_true(all(beat_wavelengths %in% waveform_obj$wavelength_spectrum$component[4:length(waveform_obj$wavelength_spectrum$component)]))

  # Check default phase is set to 0
  expect_equal(waveform_obj$phase, 0)
})
