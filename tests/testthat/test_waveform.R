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
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a waveform with only frequency spectrum
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Expectations to check waveform creation
  expect_s3_class(waveform_obj, "waveform")
  expect_equal(waveform_obj$frequency_spectrum$frequency, c(100, 200, 300))
  expect_equal(waveform_obj$frequency_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_equal(waveform_obj$wavelength_spectrum$wavelength, SPEED_OF_SOUND / c(100, 200, 300))
  expect_equal(waveform_obj$wavelength_spectrum$amplitude, c(1.0, 0.8, 0.5))
  expect_equal(waveform_obj$phase,0)
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
  expect_equal(waveform_obj$phase,0)
})

# tests/testthat/test_waveform.R

test_that("waveform plot generates correctly with time and space grid", {
  # Create a frequency_spectrum object
  f = c(100, 200, 300)
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = f,
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength_spectrum object with independent wavelengths
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / f,
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = 0
  )

  label = 'P1 3 Harmonics'
  plot <- plot(waveform_obj, label = label)
  vdiffr::expect_doppelganger(paste(label), plot)
})

test_that("waveform's indexed_spectra variable allows iteration to access all values at once, accounting for different amplitudes", {
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

  # Create the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Access the indexed_spectra variable
  indexed_spectrum <- waveform_obj$indexed_spectra

  # Expected values accounting for different amplitudes
  expected_values <-tibble::tibble(
    frequency = c(100,200,300),
    frequency_amplitude = c(1,0.8,0.5),
    wavelength = c(3.43,1.72,1.14),
    wavelength_amplitude = c(0.9,0.7,0.4)
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

  # Create waveform object with phase = 0 for simplicity
  waveform_obj <- waveform(frequency_spectrum = frequency_spectrum_obj, wavelength_spectrum = wavelength_spectrum_obj, phase = 0)

  # Set (x, t) values to test the amplitude at
  x <- 1
  t <- 0.01

  # Test the computed fundamental amplitude
  expect_equal(waveform_obj$fundamental_amplitude(x, t), -1.996,
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

  # Create the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Define test values for x (space) and t (time)
  x <- 1.0  # space in meters
  t <- 0.5  # time in seconds

  # Call the composite_amplitude function on the waveform object
  composite_amplitude_value <- waveform_obj$composite_amplitude(x, t)

  # Check that the computed composite amplitude matches the expected value
  expect_equal(composite_amplitude_value, -0.9557212, tolerance = 1e-6)
})
