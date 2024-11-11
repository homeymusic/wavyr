test_that("we can create a LinearWaveform with a frequency spectrum and speed of sound", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Calculate expected wavelengths
  expected_wavelengths <- SPEED_OF_SOUND / c(100, 200, 300)

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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Extract expected beat wavelengths using the formula from compute_beats_cpp
  wavelengths <- SPEED_OF_SOUND / frequency_spectrum_obj$component
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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Verify that base_wavelength_spectrum is included and correctly structured
  expect_s3_class(linear_waveform_obj$base_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(
    linear_waveform_obj$base_wavelength_spectrum$component,
    SPEED_OF_SOUND / frequency_spectrum_obj$component
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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Check if the combined wavelength spectrum correctly aggregates with the beat spectrum
  expect_equal(length(linear_waveform_obj$wavelength_spectrum$wavelength),
               4)
})

test_that("LinearWaveform validates amplitude correctly", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300),
    amplitude = c(-1, 0.5)  # Invalid amplitude
  )

  # Expect an error due to invalid amplitude values
  expect_error(
    linear_waveform(frequency_spectrum = frequency_spectrum_obj),
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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Access indexed_spectra
  indexed_spectrum <- linear_waveform_obj$indexed_spectra

  # Calculate expected beat wavelength (from the difference frequency, 7 Hz)
  beat_wavelength <- SPEED_OF_SOUND / 7  # Speed of sound / difference frequency
  expected_amplitude_sum <- 1.0 + 0.8  # Sum of amplitudes for 100 Hz and 107 Hz

  # Expected indexed_spectra tibble
  expected_indexed_spectrum <- tibble::tibble(
    frequency = c(NA, 100, 107),
    frequency_amplitude = c(NA, 1.0, 0.8),
    frequency_cycle_length = c(NA,1,1),
    wavelength = c(beat_wavelength, SPEED_OF_SOUND / 100, SPEED_OF_SOUND / 107),
    wavelength_amplitude = c(expected_amplitude_sum, 1.0, 0.8),
    wavelength_cycle_length = c(1,3,1)
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
    frequency_spectrum = frequency_spectrum_obj
  )

  # Access indexed_spectra
  indexed_spectrum <- linear_waveform_obj$indexed_spectra

  # Calculate expected beat wavelength (from the difference frequency, 7 Hz)
  beat_wavelength <- SPEED_OF_SOUND / 7  # Speed of sound / difference frequency
  close_waves_amplitude_sum <- 1.0 + 1.0 + 0.8 + 0.8 # Sum of close frequencies' amplitudes

  expected_indexed_spectrum <- tibble::tibble(
    frequency = c(NA, 100, 107),
    frequency_amplitude = c(NA, 1.0, 1.6),
    frequency_cycle_length = c(NA,1,1),
    wavelength = c(beat_wavelength, SPEED_OF_SOUND / 100, SPEED_OF_SOUND / 107),
    wavelength_amplitude = c(3.6,1,1.6),
    wavelength_cycle_length = c(1,3,1)
  )

    # Check that indexed_spectra matches expected values
  expect_equal(indexed_spectrum, expected_indexed_spectrum)
})

test_that("LinearWaveform fundamental_amplitude calculates the correct amplitude", {
  # Create a frequency spectrum object with frequencies and amplitudes
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Set test values for x (space) and t (time)
  x_test <- 2  # Space in meters
  t_test <- 1  # Time in seconds

  # Get the fundamental amplitude from the linear_waveform object
  fundamental_amplitude_value <- linear_waveform_obj$fundamental_amplitude(x_test, t_test)

  # Expect that the calculated value is close to the expected value
  expect_equal(fundamental_amplitude_value, 5.1, tolerance = 0.1)
})

test_that("linear_waveform correctly calculates composite_amplitude for given x and t", {
  # Create a frequency spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Define test values for x (space) and t (time)
  x <- 1.0  # space in meters
  t <- 0.5  # time in seconds

  # Call the composite_amplitude function on the linear_waveform object
  a <- linear_waveform_obj$composite_amplitude(x, t)

  expect_equal(a, -7.2, tolerance = 0.1)
})
