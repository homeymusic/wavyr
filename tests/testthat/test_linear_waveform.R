test_that("we can create a LinearWaveform with a frequency spectrum and speed of sound", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343
  )

  # Expectations to check LinearWaveform creation
  expect_s3_class(linear_waveform_obj, "linear_waveform")
  expect_s3_class(linear_waveform_obj, "waveform")
  expect_s3_class(linear_waveform_obj$frequency_spectrum, "frequency_spectrum")
  expect_s3_class(linear_waveform_obj$wavelength_spectrum, "wavelength_spectrum")
})

test_that("LinearWaveform calculates correct wavelengths for given frequencies", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343
  )

  # Calculate expected wavelengths
  expected_wavelengths <- 343 / c(100, 200, 300)

  # Check if the first three components of the wavelength spectrum match expected values
  expect_equal(
    linear_waveform_obj$wavelength_spectrum$wavelength %>% sort(),
    expected_wavelengths %>% sort()
  )
})

test_that("LinearWaveform includes beat_spectrum as a separate attribute", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343
  )

  # Extract expected beat wavelengths using the formula from compute_beats_cpp
  wavelengths <- 343 / frequency_spectrum_obj$component
  beat_wavelengths <- outer(wavelengths, wavelengths, function(x, y) (x * y) / abs(x - y)) %>%
    .[lower.tri(.)]
  beat_amplitudes <- outer(frequency_spectrum_obj$amplitude, frequency_spectrum_obj$amplitude, "+") %>%
    .[lower.tri(.)]

  # Verify that beat_spectrum contains the expected beat wavelengths and amplitudes
  expect_equal(linear_waveform_obj$beat_spectrum$component, beat_wavelengths)
  expect_equal(linear_waveform_obj$beat_spectrum$amplitude, beat_amplitudes)
})

test_that("LinearWaveform assigns correct classes and structure", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(250, 500, 1000),
    amplitude = c(1.0, 0.7, 0.4)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(frequency_spectrum = frequency_spectrum_obj)

  # Check for expected classes
  expect_s3_class(linear_waveform_obj, "linear_waveform")
  expect_s3_class(linear_waveform_obj, "waveform")
  expect_s3_class(linear_waveform_obj$frequency_spectrum, "frequency_spectrum")
  expect_s3_class(linear_waveform_obj$wavelength_spectrum, "wavelength_spectrum")
})

test_that("LinearWaveform includes base_wavelength_spectrum as a separate attribute", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300, 450),
    amplitude = c(1.0, 0.5, 0.3)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343
  )

  # Verify that base_wavelength_spectrum is included and correctly structured
  expect_s3_class(linear_waveform_obj$base_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(
    linear_waveform_obj$base_wavelength_spectrum$component,
    343 / frequency_spectrum_obj$component
  )
  expect_equal(
    linear_waveform_obj$base_wavelength_spectrum$amplitude,
    frequency_spectrum_obj$amplitude
  )
})

test_that("LinearWaveform correctly computes combined spectra", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 3/2*100),
    amplitude = c(1.0, 0.8, 0.4)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343
  )

  # Check if the combined wavelength spectrum correctly aggregates with the beat spectrum
  expect_equal(length(linear_waveform_obj$wavelength),
               5)
})

test_that("LinearWaveform validates amplitude correctly", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300),
    amplitude = c(-1, 0.5)  # Invalid amplitude
  )

  # Expect an error due to invalid amplitude values
  expect_error(
    linear_waveform(frequency_spectrum = frequency_spectrum_obj, speed_of_sound = 343),
    "must be positive"
  )
})

test_that("error is thrown if wavelength_spectrum has fewer components than frequency_spectrum in linear_waveform", {
  # Create a frequency spectrum with 3 components
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum with only 2 components
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5),
    amplitude = c(0.9, 0.7)
  )

  # Expect an error when trying to create the linear_waveform with mismatched spectra sizes
  expect_error(
    linear_waveform(
      frequency_spectrum = frequency_spectrum_obj,
      wavelength_spectrum = wavelength_spectrum_obj
    ),
    "wavelength_spectrum must have a size greater than or equal to frequency_spectrum"
  )
})

test_that("indexed_spectra includes beat wavelength with sum amplitude and NA for corresponding frequency components", {
  # Create a frequency spectrum with two components, 100 Hz and 107 Hz
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 107),
    amplitude = c(1.0, 0.8)
  )

  # Create the linear_waveform object, passing only frequency_spectrum
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343  # Speed of sound in air, m/s
  )

  # Access indexed_spectra
  indexed_spectrum <- linear_waveform_obj$indexed_spectra

  # Calculate expected beat wavelength (from the difference frequency, 7 Hz)
  beat_wavelength <- 343 / 7  # Speed of sound / difference frequency
  expected_amplitude_sum <- 1.0 + 0.8  # Sum of amplitudes for 100 Hz and 107 Hz

  # Expected indexed_spectra tibble
  expected_indexed_spectrum <- tibble::tibble(
    frequency = c(NA, 100, 107),
    frequency_amplitude = c(NA, 1.0, 0.8),
    wavelength = c(beat_wavelength, 343 / 100, 343 / 107),
    wavelength_amplitude = c(expected_amplitude_sum, 1.0, 0.8)
  )

  # Check that indexed_spectra matches expected values
  expect_equal(indexed_spectrum, expected_indexed_spectrum)
})

test_that("indexed_spectra treats frequencies within tolerance as the same and sums their amplitudes", {
  # Create a frequency spectrum with two components that are within tolerance of each other
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 107, 107.000000001),
    amplitude = c(1.0, 0.8, 0.8)
  )

  # Create the linear_waveform object, passing only frequency_spectrum
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    speed_of_sound = 343  # Speed of sound in air, m/s
  )

  # Access indexed_spectra
  indexed_spectrum <- linear_waveform_obj$indexed_spectra

  # Calculate expected beat wavelength (from the difference frequency, 7 Hz)
  beat_wavelength <- 343 / 7  # Speed of sound / difference frequency
  expected_amplitude_sum <- 0.8 + 0.8  # Sum of amplitudes for 100 Hz and 107 Hz
  close_waves_amplitude_sum <- 1.0 + 1.0 + 0.8 + 0.8 # Sum of close frequencies' amplitudes

  # Expected indexed_spectra tibble
  expected_indexed_spectrum <- tibble::tibble(
    frequency = c(NA, 100, 107),
    frequency_amplitude = c(NA, 1.0, expected_amplitude_sum),
    wavelength = c(beat_wavelength, 343 / 100, 343 / 107),
    wavelength_amplitude = c(close_waves_amplitude_sum, 1.0, expected_amplitude_sum)
  )

  # Check that indexed_spectra matches expected values
  expect_equal(indexed_spectrum, expected_indexed_spectrum)
})
