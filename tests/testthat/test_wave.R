source(testthat::test_path("helper.R"))

test_that("we can create a new wave with a frequency spectrum, wavelength spectrum, and phase", {
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

  # Define a phase for the wave
  phase <- pi / 4  # An arbitrary phase

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = phase
  )

  # Expectations to check wave creation
  expect_s3_class(wave_obj, "wave")
  expect_equal(wave_obj$phase, phase)
  expect_equal(wave_obj$frequency_spectrum$component, c(100, 200, 300))
  expect_equal(wave_obj$frequency_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_equal(sort(wave_obj$wavelength_spectrum$component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(wave_obj$wavelength_spectrum$amplitude), sort(c(1.0, 0.8, 0.5)))
})

test_that("we can create a new wave with just a frequency spectrum and it generates the wavelength spectrum automatically", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Define a phase for the wave
  phase <- pi / 4  # An arbitrary phase

  # Create the wave object without passing a wavelength_spectrum
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = phase
  )

  # Expectations to check wave creation
  expect_s3_class(wave_obj, "wave")
  expect_equal(wave_obj$phase, phase)

  # Verify that frequency spectrum was set correctly
  expect_equal(wave_obj$frequency_spectrum$component %>% sort(), c(100, 200, 300) %>% sort())
  expect_equal(wave_obj$frequency_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())

  # Verify that wavelength spectrum was generated correctly
  expected_wavelengths <- SPEED_OF_SOUND / c(100, 200, 300)
  expect_equal(wave_obj$wavelength_spectrum$component %>% sort(), expected_wavelengths %>% sort())
  expect_equal(wave_obj$wavelength_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
})

test_that("we can create a general wave with only a frequency spectrum and no wavelength spectrum or phase", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wave with only frequency spectrum
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Expectations to check wave creation
  expect_s3_class(wave_obj, "wave")
  expect_equal(wave_obj$frequency_spectrum$frequency %>% sort(), c(100, 200, 300) %>% sort())
  expect_equal(wave_obj$frequency_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
  expect_equal(
    wave_obj$wavelength_spectrum$wavelength %>% sort(),
    (SPEED_OF_SOUND / c(100, 200, 300)) %>% sort())
  expect_equal(wave_obj$wavelength_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
  expect_equal(wave_obj$phase,0)
})

test_that("wave with wavelength spectrum and frequency spectrum but no phase works as expected", {
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

  # Create the wave object without specifying phase
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Expectations to check wave creation
  expect_s3_class(wave_obj, "wave")
  expect_equal(wave_obj$frequency_spectrum$component %>% sort(), c(100, 200, 300) %>% sort())
  expect_equal(wave_obj$frequency_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
  expect_equal(wave_obj$wavelength_spectrum$component %>% sort(), c(1, 0.5, 0.33) %>% sort())
  expect_equal(wave_obj$wavelength_spectrum$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
  expect_equal(wave_obj$phase,0)
})

test_that("wave's indexed_spectra variable allows iteration to access all values at once, accounting for different amplitudes", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum with different amplitudes
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND  / c(100, 200, 300),
    amplitude = c(0.9, 0.7, 0.4)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Access the indexed_spectra variable
  indexed_spectrum <- wave_obj$indexed_spectra

  # Expected values accounting for different amplitudes
  expected_values <-tibble::tibble(
    frequency = c(100,200,300),
    frequency_amplitude = c(1,0.8,0.5),
    frequency_cycle_length = c(1,1,1),
    wavelength = c(3.49,1.75,1.16),
    wavelength_amplitude = c(0.9,0.7,0.4),
    wavelength_cycle_length = c(1,1,2)
  )

  expect_equal(indexed_spectrum,expected_values, tolerance=0.1)

})

test_that("fundamental_amplitude correctly computes amplitude for the fundamental component", {
  # Define frequency components, corresponding wavelengths, and amplitudes
  freq_components <- c(100, 200, 300)  # Frequencies in Hz
  amplitudes <- c(1.0, 0.8, 0.5)

  # Create frequency_spectrum and wavelength_spectrum objects
  frequency_spectrum_obj <- frequency_spectrum(frequency = freq_components, amplitude = amplitudes)
  wavelength_spectrum_obj <- wavelength_spectrum(wavelength = SPEED_OF_SOUND / freq_components, amplitude = amplitudes)

  # Create wave object with phase = 0 for simplicity
  wave_obj <- wave(frequency_spectrum = frequency_spectrum_obj, wavelength_spectrum = wavelength_spectrum_obj, phase = 0)

  # Set (x, t) values to test the amplitude at
  x <- 1
  t <- 0.01

  # Test the computed fundamental amplitude
  expect_equal(wave_obj$fundamental_amplitude(x, t), -4.59,
               tolerance=0.1)
})
test_that("composite_amplitude calculates correct values for given x and t", {
  # Create frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )

  # Create wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(3.43, 1.72),  # Corresponding wavelengths to 100 Hz and 200 Hz
    amplitude = c(1.0, 0.8)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Define test values for x (space) and t (time)
  x <- 1.0  # space in meters
  t <- 0.5  # time in seconds

  # Call the composite_amplitude function on the wave object
  composite_amplitude_value <- wave_obj$composite_amplitude(x, t)

  # Check that the computed composite amplitude matches the expected value
  expect_equal(composite_amplitude_value, 1.8, tolerance = 1e-6)
})

test_that("composite_amplitude only accepts scalar values for x and t", {
  # Create frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )

  # Create wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(3.43, 1.72),  # Corresponding wavelengths to 100 Hz and 200 Hz
    amplitude = c(1.0, 0.8)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Define test values for x and t as vectors
  x_vector <- c(1.0, 2.0)  # space in meters
  t_vector <- c(0.5, 1.0)  # time in seconds

  # Expect an error when vectors are passed instead of scalars
  expect_error(
    wave_obj$composite_amplitude(x_vector, t_vector),
    "x and t must be scalar values",
    fixed = TRUE
  )

  # Also test with only one of them as a vector to ensure both are checked individually
  expect_error(
    wave_obj$composite_amplitude(x_vector, 0.5),
    "x and t must be scalar values",
    fixed = TRUE
  )

  expect_error(
    wave_obj$composite_amplitude(1.0, t_vector),
    "x and t must be scalar values",
    fixed = TRUE
  )
})
test_that("fundamental_amplitude throws an error for non-scalar x or t values", {
  # Create frequency_spectrum and wavelength_spectrum objects
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = 0
  )

  # Test that passing vector values to fundamental_amplitude throws an error
  expect_error(wave_obj$fundamental_amplitude(c(1, 2), 0.01), "x and t must be scalar values")
  expect_error(wave_obj$fundamental_amplitude(1, c(0.01, 0.02)), "x and t must be scalar values")
  expect_error(wave_obj$fundamental_amplitude(c(1, 2), c(0.01, 0.02)), "x and t must be scalar values")
})
test_that('frequency_spectrum is correct for M3', {

  interval_midi = c(60,64,72)

  f = hrep::sparse_fr_spectrum(interval_midi, num_harmonics=2)

  # Create frequency_spectrum and wavelength_spectrum
  f_spectrum <- frequency_spectrum(
    frequency = f$x,
    amplitude = f$y
  )

  l_spectrum <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / f$x,
    amplitude  = f$y
  )

  # Create a dummy wave object with frequency_spectrum and wavelength_spectrum
  wave <- wave(
    frequency_spectrum = f_spectrum,
    wavelength_spectrum = l_spectrum
  )

  expect_equal(wave$indexed_spectra$frequency, f$x, tolerance=0.1)
  expect_equal(wave$indexed_spectra$frequency_cycle_length, c(1,3,1,2,1), tolerance=0.1)

})

test_that("wave plot generates correctly with time and space grid", {
  # Create a frequency_spectrum object
  f <- c(100, 200, 300)
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = f,
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength_spectrum object with independent wavelengths
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / f,
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = 0
  )

  # Generate the plot and capture it with vdiffr
  label <- 'P1 3 Harmonics'
  vdiffr::expect_doppelganger(label, function() plot(wave_obj, label = label))
})

test_that("cohenerce and modulation metrics for some dyads", {
  M3 = wave_for(framed_intervals$Major3,   num_harmonics = 2)
  m6 = wave_for(framed_intervals$minor6,   num_harmonics = 2)

  expect_true(M3$coherence  ==  m6$coherence)
  expect_true(M3$modulation == -m6$modulation)
})

test_that("cohenerce and modulation metrics for ionian and phrygian triads", {
  ionian   = wave_for(framed_triads$ionian, num_harmonics = 2)
  phrygian = wave_for(framed_triads$phrygian, num_harmonics = 2)

  expect_true(ionian$coherence == phrygian$coherence)
  expect_true(ionian$modulation == -phrygian$modulation)
})

test_that("wave computes fundamental frequency spectrum correctly", {
  # Create a frequency spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  # Check that the fundamental frequency spectrum is generated correctly
  expect_s3_class(wave_obj$fundamental_frequency_spectrum, "frequency_spectrum")
  expect_equal(
    wave_obj$fundamental_frequency_spectrum$frequency,
    frequency_spectrum_obj$fundamental_frequency
  )
  expect_equal(
    wave_obj$fundamental_frequency_spectrum$amplitude,
    sum(frequency_spectrum_obj$amplitude) + sum(wave_obj$wavelength_spectrum$amplitude)
  )
})

test_that("wave computes fundamental wavelength spectrum correctly", {
  # Create a frequency spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the wave object
  wave_obj <- wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  # Check that the fundamental wavelength spectrum is generated correctly
  expect_s3_class(wave_obj$fundamental_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(
    wave_obj$fundamental_wavelength_spectrum$wavelength,
    wave_obj$wavelength_spectrum$fundamental_wavelength
  )
  expect_equal(
    wave_obj$fundamental_wavelength_spectrum$amplitude,
    sum(frequency_spectrum_obj$amplitude) + sum(wave_obj$wavelength_spectrum$amplitude)
  )
})

# Test for adding two waves with different components
test_that("Adding two waves with different components combines components and amplitudes", {
  # Define frequency spectra with distinct components
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 150, 300),
    amplitude = c(1.0, 0.8, 0.6)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(300, 375, 900),
    amplitude = c(0.6, 0.4, 0.2)
  )

  # Create two waves
  wave_1 <- wave(frequency_spectrum = frequency_spectrum_1)
  label = "before combined w1"
  vdiffr::expect_doppelganger(label, function() plot(wave_1, label = label))

  wave_2 <- wave(frequency_spectrum = frequency_spectrum_2)
  label = "before combined w2"
  vdiffr::expect_doppelganger(label, function() plot(wave_2, label = label))

  # Add the waves
  combined_wave <- wave_1 + wave_2
  label = "combined w1 plus w2"
  vdiffr::expect_doppelganger(label, function() plot(combined_wave, label = label))

  # Expectations for combined frequency spectrum
  expect_equal(
    sort(combined_wave$frequency_spectrum$frequency),
    sort(c(100, 150, 300, 375, 900))
  )
  expect_equal(
    combined_wave$frequency_spectrum$amplitude,
    c(1.0, 0.8, 1.2, 0.4, 0.2)
  )
})

# Test for adding two waves with identical components (should sum amplitudes)
test_that("Adding two waves with identical components sums their amplitudes", {
  # Define frequency spectra with identical components
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(0.5, 0.3)
  )

  # Create the two waves
  wave_1 <- wave(frequency_spectrum = frequency_spectrum_1)
  wave_2 <- wave(frequency_spectrum = frequency_spectrum_2)

  # Add the waves
  combined_wave <- wave_1 + wave_2

  # Expectations for combined frequency spectrum
  expect_equal(
    combined_wave$frequency_spectrum$frequency,
    c(100, 200)
  )

  # The amplitudes for identical frequencies should be summed
  expected_amplitudes <- c(1.0 + 0.5, 0.8 + 0.3)
  expect_equal(
    combined_wave$frequency_spectrum$amplitude,
    expected_amplitudes
  )
})

# Test for adding waves with some overlapping and some distinct components
test_that("Adding two waves with some overlapping components correctly sums and combines", {
  # Define frequency spectra with some overlapping and some distinct components
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.6)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(200, 300, 400),
    amplitude = c(0.5, 0.3, 0.4)
  )

  # Create the two waves
  wave_1 <- wave(frequency_spectrum = frequency_spectrum_1)
  wave_2 <- wave(frequency_spectrum = frequency_spectrum_2)

  # Add the waves
  combined_wave <- wave_1 + wave_2

  # Expectations for combined frequency spectrum
  expect_equal(
    sort(combined_wave$frequency_spectrum$frequency),
    sort(c(100, 200, 300, 400))
  )

  # Amplitudes for overlapping frequencies should be summed
  expected_amplitudes <- c(1.0, 0.8 + 0.5, 0.6 + 0.3, 0.4)
  expect_equal(
    combined_wave$frequency_spectrum$amplitude,
    expected_amplitudes
  )
})

# Test for adding multiple waves together
test_that("Adding multiple waves combines components and amplitudes correctly", {
  # Define frequency spectra for three waves
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(300, 400),
    amplitude = c(0.6, 0.4)
  )
  frequency_spectrum_3 <- frequency_spectrum(
    frequency = c(200, 500),
    amplitude = c(0.3, 0.7)
  )

  # Create three waves
  wave_1 <- wave(frequency_spectrum = frequency_spectrum_1)
  wave_2 <- wave(frequency_spectrum = frequency_spectrum_2)
  wave_3 <- wave(frequency_spectrum = frequency_spectrum_3)

  # Add the waves
  combined_wave <- wave_1 + wave_2 + wave_3

  # Expectations for combined frequency spectrum
  expected_frequencies <- sort(c(100, 200, 300, 400, 500))
  expected_amplitudes <- c(1.0, 1.1, 0.6, 0.4, 0.7)  # Amplitudes for each frequency after summing

  expect_equal(sort(combined_wave$frequency_spectrum$frequency), expected_frequencies)
  expect_equal(combined_wave$frequency_spectrum$amplitude, expected_amplitudes)
})

# Test for summing a vector of waves
test_that("Summing a vector of waves combines components and amplitudes correctly, including wavelength values", {
  # Define frequency spectra for multiple waves
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(300, 200),
    amplitude = c(0.6, 0.4)
  )
  frequency_spectrum_3 <- frequency_spectrum(
    frequency = c(500, 100),
    amplitude = c(0.5, 0.3)
  )

  # Create a vector of waves
  waves <- list(
    wave(frequency_spectrum = frequency_spectrum_1),
    wave(frequency_spectrum = frequency_spectrum_2),
    wave(frequency_spectrum = frequency_spectrum_3)
  )

  # Sum the waves using Reduce
  combined_wave <- Reduce(`+`, waves)

  # Expectations for combined frequency spectrum
  expected_frequencies <- sort(c(100, 200, 300, 500))
  expected_amplitudes <- sort(c(1.3, 1.2, 0.6, 0.5))  # Expected amplitudes after summing

  # Calculate the expected wavelengths based on SPEED_OF_SOUND
  expected_wavelengths <- sort(SPEED_OF_SOUND / expected_frequencies)

  # Check that the combined wave has the correct frequencies and amplitudes
  expect_equal(sort(combined_wave$frequency_spectrum$frequency), expected_frequencies)
  expect_equal(sort(combined_wave$frequency_spectrum$amplitude), expected_amplitudes)

  # Check that the combined wave has the correct wavelengths and corresponding amplitudes
  expect_equal(sort(combined_wave$wavelength_spectrum$wavelength), expected_wavelengths)
  expect_equal(combined_wave$wavelength_spectrum$amplitude, expected_amplitudes)
})

# Test for summing a vector of waves, including a superposed_wave, combines components and amplitudes correctly
test_that("Summing a vector of waves, including a superposed_wave, combines components and amplitudes correctly, including wavelength values", {
  # Define frequency spectra for multiple waves
  frequency_spectrum_1 <- frequency_spectrum(
    frequency = c(100, 200),
    amplitude = c(1.0, 0.8)
  )
  frequency_spectrum_2 <- frequency_spectrum(
    frequency = c(300, 200),
    amplitude = c(0.6, 0.4)
  )
  frequency_spectrum_3 <- frequency_spectrum(
    frequency = c(500, 100),
    amplitude = c(0.5, 0.3)
  )

  # Create two waves and one superposed_wave
  waves <- list(
    wave(frequency_spectrum = frequency_spectrum_1),
    wave(frequency_spectrum = frequency_spectrum_2),
    superposed_wave(frequency_spectrum = frequency_spectrum_3)
  )

  # Sum the waves using Reduce
  combined_wave <- Reduce(`+`, waves)

  # Expectations for combined frequency spectrum
  expected_frequencies <- sort(c(100, 200, 300, 500))
  expected_amplitudes <- sort(c(1.3, 1.2, 0.6, 0.5))  # Expected amplitudes after summing

  # Calculate the expected wavelengths based on SPEED_OF_SOUND
  expected_wavelengths <- c(0.6984565, 0.8730706, 1.1640941 ,1.7461412 ,3.4922823)
  expected_wavelength_amplitudes <- c(0.5, 0.8, 0.6, 1.2 ,1.3)

  # Check that the combined wave has the correct frequencies and amplitudes
  expect_equal(sort(combined_wave$frequency_spectrum$frequency), expected_frequencies)
  expect_equal(sort(combined_wave$frequency_spectrum$amplitude), expected_amplitudes)

  # Check that the combined wave has the correct wavelengths and corresponding amplitudes
  expect_equal(sort(combined_wave$wavelength_spectrum$wavelength), expected_wavelengths,
               tolerance = FLOATING_POINT_TOLERANCE)
  expect_equal(combined_wave$wavelength_spectrum$amplitude, expected_wavelength_amplitudes)
})
