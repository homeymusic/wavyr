test_that("space_signal constructor creates a valid space_signal object", {
  # Create a wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(500, 1000, 1500),
    amplitude = c(0.9, 0.6, 0.4)
  )

  # Construct a space_signal object using the wavelength_spectrum
  space_signal_obj <- space_signal(wavelength_spectrum_obj)

  # Check that the object is of class "space_signal"
  expect_s3_class(space_signal_obj, "space_signal")

  # Check that the object is also of class "signal"
  expect_s3_class(space_signal_obj, "signal")

  # Check that the space_signal object contains the wavelength_spectrum object
  expect_true("spectrum" %in% names(space_signal_obj))
  expect_identical(space_signal_obj$spectrum, wavelength_spectrum_obj)
})

test_that("space_signal stores the wavelength spectrum components and amplitudes correctly", {
  # Create a wavelength_spectrum object with known components and amplitudes
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(400, 800, 1200),
    amplitude = c(0.5, 0.7, 0.3)
  )

  # Create the space_signal object
  space_signal_obj <- space_signal(wavelength_spectrum_obj)

  # Verify that the stored spectrum has correct wavelengths and amplitudes
  expect_equal(space_signal_obj$idealized_wavelength_spectrum$idealized_wavelength, c(400, 800, 1200))
  expect_equal(space_signal_obj$spectrum$idealized_wavelength, c(400, 800, 1200))
  expect_equal(space_signal_obj$spectrum$amplitude, c(0.5, 0.7, 0.3))
})

test_that("space signal has correct metadata", {
  # Create a spectrum object with known components and amplitudes
  spectrum_obj <- wavelength_spectrum(
    wavelength = 1,
    amplitude = 1
  )

  # Create the signal object
  signal_obj <- space_signal(spectrum_obj)
  # Verify that the stored spectrum has correct components and amplitudes
  expect_equal(signal_obj$plot_color, colors_homey$minor)

  expect_equal(signal_obj$physical_label, 'Space')
  expect_equal(signal_obj$spectral_label, 'Wavelength')
  expect_equal(signal_obj$observable_label, 'Amplitude')
  expect_equal(signal_obj$physical_units, 'm')
  expect_equal(signal_obj$observable_units, '')
  expect_equal(signal_obj$spectral_units, 'm')

})

test_that("space_signal constructor fails with non-wavelength_spectrum input", {
  # Try to pass a non-wavelength_spectrum input
  non_wavelength_spectrum_input <- list(frequency = c(500, 1000), amplitude = c(0.9, 0.6))

  # Expect an error when creating a space_signal with invalid input
  expect_error(space_signal(non_wavelength_spectrum_input), "Input must be of class 'wavelength_spectrum'")

  # Try to pass a non-wavelength_spectrum input
  non_wavelength_spectrum_input <- spectrum(component = c(500, 1000), amplitude = c(0.9, 0.6))

  # Expect an error when creating a space_signal with invalid input
  expect_error(space_signal(non_wavelength_spectrum_input), "Input must be of class 'wavelength_spectrum'")

})

test_that("amplitude function in space_signal works for a single-wavelength wavelength_spectrum", {
  # Create a wavelength_spectrum object with a single wavelength wavelength (500) and amplitude (1)
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1),
    amplitude = c(1)
  )

  # Create the space_signal object
  space_signal_obj <- space_signal(wavelength_spectrum_obj)

  # Test the amplitude at specific coordinates
  expect_equal(space_signal_obj$amplitude(0), 1)
  expect_equal(space_signal_obj$amplitude(0.25), 0)
  expect_equal(space_signal_obj$amplitude(0.5), -1)
  expect_equal(space_signal_obj$amplitude(0.75), 0)
  expect_equal(space_signal_obj$amplitude(1), 1)
})

test_that("signal amplitude with Feynman's 4 Hz and 5 Hz example includes expected beating and original components", {
  # Create a spectrum with Feynman's example components
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(4, 5),
    amplitude = c(1.0, 1.0)  # Equal amplitudes for simplicity
  )

  # Construct the signal object
  signal_obj <- space_signal(spectrum_obj)

  # Check that signal_obj is of class "space_signal"
  expect_s3_class(signal_obj, "space_signal")

  # Check the 1 Hz beat frequency: the combined effect at integer multiples of the beat period
  coordinate_1 <- 0  # start of beat cycle
  expected_amplitude_1 <- 2
  expect_equal(signal_obj$amplitude(coordinate_1), expected_amplitude_1)

  # Confirm a minimum at half the beat period due to out-of-phase cancellation
  coordinate_2 <- 0.25 * signal_obj$idealized_wavelength_spectrum$fundamental_wavelength  # seconds
  expected_amplitude_2 <- 1
  expect_equal(signal_obj$amplitude(coordinate_2), expected_amplitude_2,
               tolerance=0.1)

  # Confirm a minimum at half the beat period due to out-of-phase cancellation
  coordinate_3 <- 0.5 * signal_obj$idealized_wavelength_spectrum$fundamental_wavelength  # seconds
  expected_amplitude_3 <- 0
  expect_equal(signal_obj$amplitude(coordinate_3), expected_amplitude_3,
               tolerance=0.1)

  # Verify full beat cycle at 1 second
  coordinate_4 <- signal_obj$idealized_wavelength_spectrum$fundamental_wavelength  # seconds
  expected_amplitude_4 <- 2
  expect_equal(signal_obj$amplitude(coordinate_4), expected_amplitude_4, tolerance = 0.1)
})

test_that("signal plot matches expected output for specified coordinate range", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c( 4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  space_signal_obj <- space_signal(spectrum_obj)
  # Check that the object is of class "space_signal"
  expect_s3_class(space_signal_obj, "space_signal")


  # Define label and coordinate range
  label <- "Feynman's Beats (4 Hz and 5 Hz)"
  coordinate_range <- c(0, 2)  # Range for the plot

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj, coordinate_range = coordinate_range))
})

test_that("signal plot defaults to 3 full cycles when coordinate_range is not provided", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c( 4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  space_signal_obj <- space_signal(spectrum_obj)
  # Check that the object is of class "space_signal"
  expect_s3_class(space_signal_obj, "space_signal")


  # Define label
  label <- "Feynman's Beats (4 Hz and 5 Hz) with 3 Full Cycles"

  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj))

  # Alternatively, check that the plot range indeed covers 3 full cycles
  # You could inspect the axis limits or other aspects of the plot here.
})

test_that("space signal plot of feynman waves with superposition", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- frequency_spectrum(
    frequency = c( 4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  superposed_wave = superposed_wave(spectrum_obj)

  # Create the signal object from the spectrum
  space_signal_obj <- superposed_wave$idealized_wavelength_spectrum %>% space_signal()
  # Check that the object is of class "space_signal"
  expect_s3_class(space_signal_obj, "space_signal")


  # Define label
  label <- "Feynman's Beats Superposed 1/4 cycle"

  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj,
                                                     title = label,
                                                     number_of_cycles = 1/4,
                                                     resolution=1001))

  label <- "Feynman's Beats Superposed 1 cycle"
  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj,
                                                     title = label,
                                                     number_of_cycles = 1,
                                                     resolution=1001))

  label <- "Feynman's Beats Superposed 3 cycle"
  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj,
                                                     title = label,
                                                     number_of_cycles = 3,
                                                     resolution=1001))

  label <- "Feynman's Beats Superposed 10 cycle"
  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj,
                                                     title = label,
                                                     number_of_cycles = 10,
                                                     resolution=1001))

  label <- "Feynman's Beats Superposed 100 cycle"
  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(space_signal_obj,
                                                     title = label,
                                                     number_of_cycles = 100,
                                                     resolution=1001))

})

test_that("detailed space signal plots match expected output", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(4, 5),      # Frequencies in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  signal_obj <- space_signal(spectrum_obj)

  expect_equal(signal_obj$plot_color, colors_homey$minor)

  # Define label and coordinate range
  label <- "Feynman's Beats Details"

  plot_details.signal(signal_obj)

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj))
})

test_that("5 random wavelengths looks intersting", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(60,64,67,79,72) %>% midi_to_freq(),
    amplitude =1 / (1:5)
  )

  # Create the signal object from the spectrum
  signal_obj <- space_signal(spectrum_obj)

  expect_equal(signal_obj$plot_color, colors_homey$minor)

  # Define label and coordinate range
  label <- "Super Major Chord"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj))
})

test_that("Framed M3 Detail Plots", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  framed_M3 = c(60,64,72)
  framed_M3_2_harmonics = c(framed_M3, 12 + framed_M3)
  spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / framed_M3_2_harmonics %>% midi_to_freq(),
    amplitude  = 1 / (1:6)
  )

  # Create the signal object from the spectrum
  signal_obj <- space_signal(spectrum_obj)

  expect_equal(signal_obj$plot_color, colors_homey$minor)

  # Define label and coordinate range
  label <- "Framed M3"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj))
})

