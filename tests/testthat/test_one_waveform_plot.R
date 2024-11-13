test_that("detailed: waveform plot generates correctly with time and space grid", {
  label <- 'C4 with 5 Harmonics'
  # Create a frequency_spectrum object
  f =  60 %>% hrep::sparse_fr_spectrum(num_harmonics=5)
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = f$x,
    amplitude = f$y
  )

  # Confirm fundamental properties in frequency_spectrum
  expect_true(!is.null(frequency_spectrum_obj$fundamental_frequency))
  expect_true(!is.null(frequency_spectrum_obj$fundamental_cycle_length))

  # Create a wavelength_spectrum object with independent wavelengths
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / f$x,
    amplitude = f$y
  )

  # Confirm fundamental properties in wavelength_spectrum
  expect_true(!is.null(wavelength_spectrum_obj$fundamental_wavelength))
  expect_true(!is.null(wavelength_spectrum_obj$fundamental_cycle_length))

  # Create the waveform object
  waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj,
    wavelength_spectrum = wavelength_spectrum_obj,
    phase = 0
  )

  # Verify that waveform_obj has the expected properties
  expect_true(!is.null(waveform_obj$frequency_spectrum))
  expect_true(!is.null(waveform_obj$wavelength_spectrum))
  expect_true(!is.null(waveform_obj$indexed_spectra))
  expect_true(!is.null(waveform_obj$composite_amplitude))
  expect_true(!is.null(waveform_obj$fundamental_amplitude))

  # Check if composite_amplitude function runs without errors and returns a numeric value
  expect_type(waveform_obj$composite_amplitude(0, 0), "double")

  # Check if fundamental_amplitude function runs without errors and returns a numeric value
  expect_type(waveform_obj$fundamental_amplitude(0, 0), "double")

  expect_true(inherits(waveform_obj, "linear_waveform"))
  expect_true(inherits(waveform_obj, "waveform"))

  vdiffr::expect_doppelganger(label, function() plot(waveform_obj, label = label))
})
