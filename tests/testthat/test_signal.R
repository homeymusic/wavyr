test_that("signal constructor creates a valid signal object", {
  # Create a spectrum object
  spectrum_obj <- spectrum(
    idealized_component = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Construct a signal object using the spectrum
  signal_obj <- signal(spectrum_obj)

  # Check that the object is of class "signal"
  expect_s3_class(signal_obj, "signal")

  # Check that the signal object contains the spectrum object
  expect_true("spectrum" %in% names(signal_obj))
  expect_identical(signal_obj$spectrum, spectrum_obj)
})

test_that("signal stores the spectrum components and amplitudes correctly", {
  # Create a spectrum object with known components and amplitudes
  f = c(100, 125, 150)
  a = c(1, 0.75, 0.5)
  spectrum_obj <- spectrum(
    idealized_component = f,
    amplitude = a
  )

  # Create the signal object
  signal_obj <- signal(spectrum_obj)
  # Verify that the stored spectrum has correct components and amplitudes
  expect_equal(signal_obj$spectrum$idealized_component, f)
  expect_equal(signal_obj$spectrum$amplitude, a)
})

test_that("signal has correct metadata", {
  # Create a spectrum object with known components and amplitudes
  spectrum_obj <- spectrum(
    idealized_component = 1,
    amplitude = 1
  )

  # Create the signal object
  signal_obj <- signal(spectrum_obj)
  # Verify that the stored spectrum has correct components and amplitudes
  expect_equal(signal_obj$plot_color, colors_homey$neutral)
  expect_equal(signal_obj$physical_label, 'Coordinate')
  expect_equal(signal_obj$spectral_label, 'Spectrum')
  expect_equal(signal_obj$observable_label, 'Amplitude')
  expect_equal(signal_obj$physical_units, 'Natural Coordinate Units')
  expect_equal(signal_obj$observable_units, 'Natural Amplitude Units')
  expect_equal(signal_obj$spectral_units, 'Cycles per Natural Coordinate Unit')

})

test_that("signal constructor fails with non-spectrum input", {
  # Try to pass a non-spectrum input
  non_spectrum_input <- list(component = c(100, 200), amplitude = c(1.0, 0.5))

  # Expect an error when creating a signal with invalid input
  expect_error(signal(non_spectrum_input), "Input must be of class 'spectrum'")
})

test_that("amplitude function in signal class works for a single-component spectrum", {
  # Create a spectrum object with a single frequency component (1 Hz) and amplitude (1)
  spectrum_obj <- spectrum(
    idealized_component = c(1),  # 1 Hz frequency
    amplitude = c(1)   # Amplitude of 1
  )

  # Create the signal object
  signal_obj <- signal(spectrum_obj)

  # Test the amplitude at specific coordinates without recalculating expected results
  expect_equal(signal_obj$amplitude(0), 1)         # cos(0) = 1
  expect_equal(signal_obj$amplitude(0.25), 0)      # cos(pi/2) = 0
  expect_equal(signal_obj$amplitude(0.5), -1)      # cos(pi) = -1
  expect_equal(signal_obj$amplitude(0.75), 0)      # cos(3*pi/2) = 0
  expect_equal(signal_obj$amplitude(1), 1)         # cos(2*pi) = 1
})

test_that("signal amplitude with Feynman's 4 Hz and 5 Hz example includes expected beating and original components", {
  # Create a spectrum with Feynman's example components
  spectrum_obj <- spectrum(
    idealized_component = c(4, 5),  # Frequency components in Hz
    amplitude = c(1.0, 1.0)  # Equal amplitudes for simplicity
  )

  # Construct the signal object
  signal_obj <- signal(spectrum_obj)

  # Check the 1 Hz beat frequency: the combined effect at integer multiples of the beat period
  coordinate_1 <- 0  # start of beat cycle
  expected_amplitude_1 <- 2  # cos(0) + cos(0) = 1 + 1 = 2
  expect_equal(signal_obj$amplitude(coordinate_1), expected_amplitude_1)

  # Confirm a minimum at half the beat period due to out-of-phase cancellation
  coordinate_2 <- 0.5  # seconds
  expected_amplitude_2 <- 0  # cos(2 * pi * 4 * 0.5) + cos(2 * pi * 5 * 0.5) = -1 + 1 = 0
  expect_equal(signal_obj$amplitude(coordinate_2), expected_amplitude_2)

  # Verify full beat cycle at 1 second
  coordinate_5 <- 1  # seconds
  expected_amplitude_5 <- 2  # cos(2 * pi * 4 * 1) + cos(2 * pi * 5 * 1) = 1 + 1 = 2
  expect_equal(signal_obj$amplitude(coordinate_5), expected_amplitude_5)
})

test_that("signal plot matches expected output for specified coordinate range", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- spectrum(
    idealized_component = c(4, 5),      # Frequencies in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  signal_obj <- signal(spectrum_obj)
  expect_equal(spectrum_obj$extent_rate, EXTENT_RATE$rate)
  expect_equal(spectrum_obj$rationalized_cycles_per_reference, 4)
  expect_equal(spectrum_obj$rationalized_fundamental_component, 1)
  expect_equal(spectrum_obj$rationalized_extent, 1)

  # Define label and coordinate range
  label <- "Feynman's Beats (4 Hz and 5 Hz)"
  coordinate_range <- c(0, 2)  # Range for the plot

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot(signal_obj, coordinate_range = coordinate_range))
})

test_that("signal plot defaults to 3 full cycles when coordinate_range is not provided", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- spectrum(
    idealized_component = c(4, 5),      # Frequencies in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  signal_obj <- signal(spectrum_obj)

  # Define label
  label <- "Feynman's Beats (4 Hz and 5 Hz) with 3 Full Cycles"

  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(signal_obj))
})

test_that("detailed signal plots match expected output", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- spectrum(
    idealized_component = c(4, 5),      # Frequencies in Hz
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  signal_obj <- signal(spectrum_obj)

  expect_equal(signal_obj$plot_color, colors_homey$neutral)

  # Define label and coordinate range
  label <- "Feynman's Beats Details"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj))
})
test_that("10 random frequencies looks intersting", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- spectrum(
    idealized_component = c(60,64,67,79,72) %>% midi_to_freq(),
    amplitude =1 / (1:5)
  )

  # Create the signal object from the spectrum
  signal_obj <- signal(spectrum_obj)

  expect_equal(signal_obj$plot_color, colors_homey$neutral)

  # Define label and coordinate range
  label <- "Super Major Chord"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj, resolution = 2000))
})
