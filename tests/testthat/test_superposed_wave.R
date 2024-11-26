test_that("we can create a LinearWaveform with a frequency spectrum and speed of sound", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Expectations to check LinearWaveform creation
  expect_s3_class(superposed_wave_obj, "superposed_wave")
  expect_s3_class(superposed_wave_obj, "wave")
  expect_s3_class(superposed_wave_obj$frequency_spectrum, "frequency_spectrum")
  expect_s3_class(superposed_wave_obj$idealized_wavelength_spectrum, "wavelength_spectrum")
})

test_that("LinearWaveform calculates correct wavelengths for given frequencies", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Calculate expected wavelengths
  expected_wavelengths <- SPEED_OF_SOUND / c(100, 200, 300)

  # Check if the first three components of the wavelength spectrum match expected values
  expect_equal(
    superposed_wave_obj$idealized_wavelength_spectrum$idealized_wavelength %>% sort(),
    expected_wavelengths %>% sort()
  )
})

test_that("LinearWaveform includes beat_wavelength_spectrum as a separate attribute", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Verify that beat_wavelength_spectrum contains the expected beat wavelengths and amplitudes
  expect_equal(superposed_wave_obj$beat_wavelength_spectrum$component, c(1.746141, 3.492282),
               tolerance=FLOATING_POINT_TOLERANCE)
  expect_equal(superposed_wave_obj$beat_wavelength_spectrum$amplitude, c(1.5,3.1),
               tolerance=FLOATING_POINT_TOLERANCE)
})

test_that("LinearWaveform assigns correct classes and structure", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(250, 500, 1000),
    amplitude = c(1.0, 0.7, 0.4)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(frequency_spectrum = frequency_spectrum_obj)

  # Check for expected classes
  expect_s3_class(superposed_wave_obj, "superposed_wave")
  expect_s3_class(superposed_wave_obj, "wave")
  expect_s3_class(superposed_wave_obj$frequency_spectrum, "frequency_spectrum")
  expect_s3_class(superposed_wave_obj$idealized_wavelength_spectrum, "wavelength_spectrum")
})

test_that("LinearWaveform includes base_wavelength_spectrum as a separate attribute", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300, 450),
    amplitude = c(1.0, 0.5, 0.3)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Verify that base_wavelength_spectrum is included and correctly structured
  expect_s3_class(superposed_wave_obj$base_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(
    sort(superposed_wave_obj$base_wavelength_spectrum$component),
    sort(SPEED_OF_SOUND / frequency_spectrum_obj$component)
  )
  expect_equal(
    sort(superposed_wave_obj$base_wavelength_spectrum$amplitude),
    sort(frequency_spectrum_obj$amplitude)
  )
})

test_that("LinearWaveform correctly computes combined spectra", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 3/2*100),
    amplitude = c(1.0, 0.8, 0.4)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Check if the combined wavelength spectrum correctly aggregates with the beat spectrum
  expect_equal(length(superposed_wave_obj$idealized_wavelength_spectrum$idealized_wavelength),
               4)
})

test_that("LinearWaveform validates amplitude correctly", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300),
    amplitude = c(-1, 0.5)  # Invalid amplitude
  )

  # Expect an error due to invalid amplitude values
  expect_error(
    superposed_wave(frequency_spectrum = frequency_spectrum_obj),
    "must be positive"
  )
})

test_that("error is thrown if wavelength_spectrum has fewer components than frequency_spectrum in superposed_wave", {
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

  # Expect an error when trying to create the superposed_wave with mismatched spectra sizes
  expect_error(
    superposed_wave(
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

  # Create the superposed_wave object, passing only frequency_spectrum
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Access indexed_spectra
  indexed_spectrum <- superposed_wave_obj$indexed_spectra

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
    wavelength_cycle_length = c(3,1,1)
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

  # Create the superposed_wave object, passing only frequency_spectrum
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Access indexed_spectra
  indexed_spectrum <- superposed_wave_obj$indexed_spectra

  # Calculate expected beat wavelength (from the difference frequency, 7 Hz)
  beat_wavelength <- SPEED_OF_SOUND / 7  # Speed of sound / difference frequency
  close_waves_amplitude_sum <- 1.0 + 1.0 + 0.8 + 0.8 # Sum of close frequencies' amplitudes

  expected_indexed_spectrum <- tibble::tibble(
    frequency = c(NA, 100, 107),
    frequency_amplitude = c(NA, 1.0, 1.6),
    frequency_cycle_length = c(NA,1,1),
    wavelength = c(beat_wavelength, SPEED_OF_SOUND / 100, SPEED_OF_SOUND / 107),
    wavelength_amplitude = c(2.6,1,1.6),
    wavelength_cycle_length = c(3,1,1)
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
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Set test values for x (space) and t (time)
  x_test <- 2  # Space in meters
  t_test <- 1  # Time in seconds

  # Get the fundamental amplitude from the superposed_wave object
  fundamental_amplitude_value <- superposed_wave_obj$fundamental_amplitude(x_test, t_test)

  # Expect that the calculated value is close to the expected value
  expect_equal(fundamental_amplitude_value, 9.2, tolerance = 0.1)
})

test_that("superposed_wave correctly calculates composite_amplitude for given x and t", {
  # Create a frequency spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Define test values for x (space) and t (time)
  x <- 1.0  # space in meters
  t <- 0.5  # time in seconds

  # Call the composite_amplitude function on the superposed_wave object
  a <- superposed_wave_obj$composite_amplitude(x, t)

  expect_equal(a, -3, tolerance = 0.1)
})

test_that("plot.superposed_wave renders without errors for basic superposed_wave", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Capture the plot with vdiffr
  vdiffr::expect_doppelganger("basic superposed_wave plot", function() {
    plot(superposed_wave_obj, label = "Test LinearWaveform")
  })
})

test_that("plot.superposed_wave renders beat_wavelength_spectrum overlay correctly", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(150, 300, 450),
    amplitude = c(1.0, 0.5, 0.3)
  )

  # Create a LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Capture the plot with beat_wavelength_spectrum overlay
  vdiffr::expect_doppelganger("superposed_wave with beat spectrum overlay", function() {
    plot(superposed_wave_obj, label = "Test LinearWaveform with Beat")
  })
})

test_that("plot.superposed_wave maintains original appearance without beat spectrum overlay", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )

  # Create a LinearWaveform object
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Capture the plot without beat spectrum overlay
  vdiffr::expect_doppelganger("superposed_wave plot without beat overlay", function() {
    plot(superposed_wave_obj, label = "LinearWaveform No Beat Overlay")
  })
})

test_that("plot.superposed_wave displays base and beat wavelength spectra as distinct overlays", {
  # Create a frequency_spectrum object with close frequencies
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 107, 200),
    amplitude = c(1.0, 0.5, 0.3)
  )

  # Create the LinearWaveform object with overlapping base and beat spectra
  superposed_wave_obj <- superposed_wave(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Capture the plot with both base and beat spectra overlayed distinctly
  vdiffr::expect_doppelganger("superposed_wave with distinct base and beat overlays", function() {
    plot(superposed_wave_obj, label = "Test with Base and Beat Overlays")
  })
})
test_that("wavelength spectrum with beats makes sense", {
  l = SPEED_OF_SOUND / c(4,5, abs(4-5))
  a = c(1,1,1)

  wavelength_spectrum_with_beats = wavelength_spectrum(
    idealized_wavelength = l,
    amplitude  = a
  )

  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- frequency_spectrum(
    idealized_frequency = c( 4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )
  superposed_wave = superposed_wave(spectrum_obj)
  expected_wavelength_spectrum = superposed_wave$idealized_wavelength_spectrum
  expect_equal(wavelength_spectrum_with_beats$idealized_wavelength, expected_wavelength_spectrum$idealized_wavelength)

  spectrum_obj = superposed_wave$idealized_wavelength_spectrum

  beat_idealized_wavelength = spectrum_obj$idealized_wavelength[1] * spectrum_obj$idealized_wavelength[2] /
    abs(spectrum_obj$idealized_wavelength[1] - spectrum_obj$idealized_wavelength[2])

  expect_equal(spectrum_obj$idealized_wavelength, c(69.84565, 87.30706, beat_wavelength), tolerance = 0.1)
  expect_equal(spectrum_obj$inverted, T)
  expect_equal(spectrum_obj$rationalized_cycles_per_reference, 4)
  expect_equal(spectrum_obj$fundamental_wavelength, 349.22, tolerance = 0.1)
  expect_equal(spectrum_obj$fundamental_cycle_length, 349.22, tolerance = 0.1)
})
test_that("wavelength plot of feynman waves with superposition", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- frequency_spectrum(
    idealized_frequency = c( 4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  superposed_wave = superposed_wave(spectrum_obj)

  label <- "Feynman's Beats Superposed 3 cycle"
  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(superposed_wave$idealized_wavelength_spectrum,
                                                     title = label))

})
