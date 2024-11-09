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
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength_spectrum object with independent wavelengths
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(2.0, 1.0, 0.67),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = 0
  )

  # Capture the plot using vdiffr for consistency checks
  vdiffr::expect_doppelganger("Waveform",
                              plot(waveform_obj,
                                   time_range = c(0, 10),
                                   space_range = c(0, 10),
                                   resolution = 50)
  )
})

test_that("waveform's indexed_spectra variable allows iteration to access all values at once, accounting for different amplitudes", {
  # Create a frequency spectrum
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum with different amplitudes
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
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
  expected_values <- list(
    list(frequency = 100, wavelength = 1, frequency_amplitude = 1.0, wavelength_amplitude = 0.9),
    list(frequency = 200, wavelength = 0.5, frequency_amplitude = 0.8, wavelength_amplitude = 0.7),
    list(frequency = 300, wavelength = 0.33, frequency_amplitude = 0.5, wavelength_amplitude = 0.4)
  )

  for (i in seq_len(nrow(indexed_spectrum))) {
    row_values <- indexed_spectrum[i, ]
    expect_equal(row_values$frequency, expected_values[[i]]$frequency)
    expect_equal(row_values$wavelength, expected_values[[i]]$wavelength)
    expect_equal(row_values$frequency_amplitude, expected_values[[i]]$frequency_amplitude)
    expect_equal(row_values$wavelength_amplitude, expected_values[[i]]$wavelength_amplitude)
  }
})

test_that("error is thrown if frequency_spectrum and wavelength_spectrum have different sizes", {
  # Create a frequency spectrum with 3 components
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a wavelength spectrum with 2 components
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5),
    amplitude = c(0.9, 0.7)
  )

  # Expect an error when trying to create the waveform with mismatched spectra sizes
  expect_error(
    waveform(
      frequency_spectrum = frequency_spectrum_obj,
      wavelength_spectrum = wavelength_spectrum_obj
    ),
    "frequency_spectrum and wavelength_spectrum must have the same number of components"
  )
})
test_that("fundamental_amplitude correctly computes amplitude for the fundamental component", {
  # Define frequency components, corresponding wavelengths, and amplitudes
  freq_components <- c(100, 200, 300)  # Frequencies in Hz
  amplitudes <- c(1.0, 0.8, 0.5)
  speed_of_sound <- 343  # Speed of sound in m/s

  # Create frequency_spectrum and wavelength_spectrum objects
  frequency_spectrum_obj <- frequency_spectrum(frequency = freq_components, amplitude = amplitudes)
  wavelength_spectrum_obj <- wavelength_spectrum(wavelength = speed_of_sound / freq_components, amplitude = amplitudes)

  # Create waveform object with phase = 0 for simplicity
  waveform_obj <- waveform(frequency_spectrum = frequency_spectrum_obj, wavelength_spectrum = wavelength_spectrum_obj, phase = 0)

  # Set (x, t) values to test the amplitude at
  x <- 1
  t <- 0.01

  # Use the waveform's own fundamental frequency and wavelength
  fundamental_frequency <- waveform_obj$frequency_spectrum$fundamental_frequency
  fundamental_wavelength <- waveform_obj$wavelength_spectrum$fundamental_wavelength

  # Calculate the expected amplitude based on the fundamental component of the waveform
  expected_fundamental_amplitude <- amplitudes[1] * cos((2 * pi / fundamental_wavelength) * x - (2 * pi * fundamental_frequency) * t)

  # Test the computed fundamental amplitude
  expect_equal(waveform_obj$fundamental_amplitude(x, t), expected_fundamental_amplitude)
})
