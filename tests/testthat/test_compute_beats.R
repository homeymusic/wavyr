test_that("compute_beats_cpp handles basic beat calculation for extent", {
  # Input vectors
  component <- c(1.0, 0.5, 0.33) # Example: wavelength (extent)
  amplitude <- c(1.0, 0.8, 0.5)

  # Call the compute_beats_cpp function for extent
  result <- compute_beats_cpp(component, amplitude, EXTENT_RATE$extent)

  # Check the structure of the result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("component", "amplitude") %in% names(result)))

  # Check the number of results (3 beats from 3 pairs)
  expect_equal(nrow(result), 3)

  # Expected values for extent-based calculation
  expected_values <- c(
    (1.0 * 0.5) / abs(1.0 - 0.5),
    (1.0 * 0.33) / abs(1.0 - 0.33),
    (0.5 * 0.33) / abs(0.5 - 0.33)
  )
  expected_amplitudes <- c(1.8, 1.5, 1.3)

  # Compare calculated values to expected values
  expect_equal(result$component, expected_values, tolerance = 1e-6)
  expect_equal(result$amplitude, expected_amplitudes, tolerance = 1e-6)
})

test_that("compute_beats_cpp handles basic beat calculation for rate", {
  # Input vectors
  component <- c(100, 200, 300) # Example: frequency (rate)
  amplitude <- c(1.0, 0.8, 0.5)

  # Call the compute_beats_cpp function for rate
  result <- compute_beats_cpp(component, amplitude, EXTENT_RATE$rate)

  # Check the structure of the result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("component", "amplitude") %in% names(result)))

  # Check the number of results (3 beats from 3 pairs)
  expect_equal(nrow(result), 3)

  # Expected values for rate-based calculation
  expected_values <- c(
    abs(100 - 200),
    abs(100 - 300),
    abs(200 - 300)
  )
  expected_amplitudes <- c(1.8, 1.5, 1.3)

  # Compare calculated values to expected values
  expect_equal(result$component, expected_values, tolerance = 1e-6)
  expect_equal(result$amplitude, expected_amplitudes, tolerance = 1e-6)
})

test_that("compute_beats_cpp returns empty DataFrame for single element", {
  # Single-element input
  component <- c(1.0)
  amplitude <- c(1.0)

  # Call the compute_beats_cpp function
  result <- compute_beats_cpp(component, amplitude, EXTENT_RATE$extent)

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("compute_beats_cpp returns empty DataFrame for empty input", {
  # Empty input vectors
  component <- numeric(0)
  amplitude <- numeric(0)

  # Call the compute_beats_cpp function
  result <- compute_beats_cpp(component, amplitude, EXTENT_RATE$extent)

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("compute_beats_cpp returns an empty beat spectrum for very small frequency differences", {
  # Define frequency spectrum with very close frequencies
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100, 100.000001),
    amplitude = c(1.0, 0.8)
  )

  # Set a minimum frequency difference tolerance
  min_frequency_difference <- 1e-6

  # Calculate beat spectrum using compute_beats_cpp with very close frequencies
  beat_spectrum <- compute_beats_cpp(
    component = SPEED_OF_SOUND / frequency_spectrum_obj$idealized_frequency,
    amplitude = frequency_spectrum_obj$amplitude,
    EXTENT_RATE$extent
  )

  # Test that beat_spectrum is empty
  expect_true(nrow(beat_spectrum) == 0)
})
