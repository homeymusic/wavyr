# tests/testthat/test_linear_waveform.R

test_that("we can create a LinearWaveform with frequency spectrum, and it generates wavelength spectrum and beat wavelengths", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(frequency_spectrum = frequency_spectrum_obj, speed_of_sound = 343)

  # Check LinearWaveform creation
  expect_s3_class(linear_waveform_obj, "LinearWaveform")

  # Check that wavelength components match expected values
  expected_wavelengths <- 343 / c(100, 200, 300)
  expect_equal(linear_waveform_obj$wavelength_spectrum$component[1:3], expected_wavelengths)

  # Check that beat wavelengths are correctly added
  beat_wavelengths <- abs(outer(expected_wavelengths, expected_wavelengths, "-"))
  beat_wavelengths <- beat_wavelengths[lower.tri(beat_wavelengths)]
  expect_true(all(beat_wavelengths %in% linear_waveform_obj$wavelength_spectrum$component[4:length(linear_waveform_obj$wavelength_spectrum$component)]))
})
