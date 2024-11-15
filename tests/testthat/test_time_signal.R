test_that("time_signal constructor creates a valid time_signal object", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(2, 4, 8),
    amplitude = c(0.9, 0.6, 0.4)
  )

  # Construct a time_signal object using the frequency_spectrum
  time_signal_obj <- time_signal(frequency_spectrum_obj)

  # Check that the object is of class "time_signal"
  expect_s3_class(time_signal_obj, "time_signal")

  # Check that the object is also of class "signal"
  expect_s3_class(time_signal_obj, "signal")

  # Check that the time_signal object contains the frequency_spectrum object
  expect_true("spectrum" %in% names(time_signal_obj))
  expect_identical(time_signal_obj$spectrum, frequency_spectrum_obj)
})

test_that("time_signal stores the frequency spectrum components and amplitudes correctly", {
  # Create a frequency_spectrum object with known components and amplitudes
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(2, 4, 8),
    amplitude = c(0.5, 0.7, 0.3)
  )

  # Create the time_signal object
  time_signal_obj <- time_signal(frequency_spectrum_obj)

  # Verify that the stored spectrum has correct frequencies and amplitudes
  expect_equal(time_signal_obj$frequency_spectrum$frequency, c(2, 4, 8))
  expect_equal(time_signal_obj$spectrum$frequency, c(2, 4, 8))
  expect_equal(time_signal_obj$spectrum$amplitude, c(0.5, 0.7, 0.3))
})

test_that("time_signal constructor fails with non-frequency_spectrum input", {
  # Try to pass a non-frequency_spectrum input
  non_frequency_spectrum_input <- list(frequency = c(4, 8), amplitude = c(0.9, 0.6))

  # Expect an error when creating a time_signal with invalid input
  expect_error(time_signal(non_frequency_spectrum_input), "Input must be of class 'frequency_spectrum'")
})

test_that("amplitude function in time_signal works for a single-frequency frequency_spectrum", {
  # Create a frequency_spectrum object with a single frequency (1 Hz) and amplitude (1)
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(1),
    amplitude = c(1)
  )

  # Create the time_signal object
  time_signal_obj <- time_signal(frequency_spectrum_obj)

  # Test the amplitude at specific times
  expect_equal(time_signal_obj$amplitude(0), 1)
  expect_equal(time_signal_obj$amplitude(0.25), 0)
  expect_equal(time_signal_obj$amplitude(0.5), -1)
  expect_equal(time_signal_obj$amplitude(0.75), 0)
  expect_equal(time_signal_obj$amplitude(1), 1)
})

test_that("signal amplitude with Feynman's 4 Hz and 5 Hz example includes expected beating and original components", {
  # Create a spectrum with Feynman's example components
  spectrum_obj <- frequency_spectrum(
    frequency = c(4, 5),
    amplitude = c(1.0, 1.0)  # Equal amplitudes for simplicity
  )

  # Construct the signal object
  signal_obj <- time_signal(spectrum_obj)

  # Check that signal_obj is of class "time_signal"
  expect_s3_class(signal_obj, "time_signal")

  # Check the 1 Hz beat frequency: the combined effect at integer multiples of the beat period
  coordinate_1 <- 0  # start of beat cycle
  expected_amplitude_1 <- 2
  expect_equal(signal_obj$amplitude(coordinate_1), expected_amplitude_1)

  # Confirm a minimum at half the beat period due to out-of-phase cancellation
  coordinate_2 <- 0.5  # seconds
  expected_amplitude_2 <- 0
  expect_equal(signal_obj$amplitude(coordinate_2), expected_amplitude_2)

  # Verify full beat cycle at 1 second
  coordinate_3 <- 1  # seconds
  expected_amplitude_3 <- 2
  expect_equal(signal_obj$amplitude(coordinate_3), expected_amplitude_3)
})

test_that("signal plot matches expected output for specified coordinate range", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- frequency_spectrum(
    frequency = c(4, 5),
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  time_signal_obj <- time_signal(spectrum_obj)

  # Define label and coordinate range
  label <- "Feynman's Beats (4 Hz and 5 Hz)"
  coordinate_range <- c(0, 2)  # Range for the plot

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot(time_signal_obj, label = label, coordinate_range = coordinate_range))
})

test_that("signal plot defaults to 3 full cycles when coordinate_range is not provided", {
  # Create a spectrum object with Feynman's example frequencies (4 Hz and 5 Hz)
  spectrum_obj <- frequency_spectrum(
    frequency = c(4, 5),
    amplitude = c(1.0, 1.0)   # Equal amplitudes for both components
  )

  # Create the signal object from the spectrum
  time_signal_obj <- time_signal(spectrum_obj)

  # Define label
  label <- "Feynman's Beats (4 Hz and 5 Hz) with 3 Full Cycles"

  # Define the expected coordinate range for 3 full cycles
  coordinate_range_expected <- c(0, 0.75)

  # Capture the plot with vdiffr and check the default behavior
  vdiffr::expect_doppelganger(label, function() plot(time_signal_obj, label = label))
})
