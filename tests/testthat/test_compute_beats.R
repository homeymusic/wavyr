# tests/testthat/test_compute_beats_cpp.R

test_that("compute_beats_cpp handles basic beat calculation", {
  # Input vectors
  wavelength <- c(1.0, 0.5, 0.33)
  amplitude <- c(1.0, 0.8, 0.5)

  # Call the compute_beats_cpp function
  result <- compute_beats_cpp(wavelength, amplitude)

  # Check the structure of the result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("wavelength", "amplitude") %in% names(result)))

  # Check the number of results (3 beats from 3 pairs)
  expect_equal(nrow(result), 3)

  # Expected values based on the input
  expected_wavelengths <- c(
    (1.0 * 0.5) / abs(1.0 - 0.5),
    (1.0 * 0.33) / abs(1.0 - 0.33),
    (0.5 * 0.33) / abs(0.5 - 0.33)
  )
  expected_amplitudes <- c(1.8, 1.5, 1.3)

  # Compare calculated values to expected values
  expect_equal(result$wavelength, expected_wavelengths, tolerance = 1e-6)
  expect_equal(result$amplitude, expected_amplitudes, tolerance = 1e-6)
})

test_that("compute_beats_cpp returns empty DataFrame for single element", {
  # Single-element input
  wavelength <- c(1.0)
  amplitude <- c(1.0)

  # Call the compute_beats_cpp function
  result <- compute_beats_cpp(wavelength, amplitude)

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("compute_beats_cpp returns empty DataFrame for empty input", {
  # Empty input vectors
  wavelength <- numeric(0)
  amplitude <- numeric(0)

  # Call the compute_beats_cpp function
  result <- compute_beats_cpp(wavelength, amplitude)

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

# tests/testthat/test_compute_beats_cpp.R

test_that("compute_beats_cpp returns an empty beat spectrum for very small frequency differences", {
  # Define frequency spectrum with very close frequencies
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 100.000001),
    amplitude = c(1.0, 0.8)
  )

  # Set a minimum frequency difference tolerance
  min_frequency_difference <- 1e-6

  # Calculate beat spectrum using compute_beats_cpp with very close frequencies
  beat_spectrum <- compute_beats_cpp(
    wavelength = 343 / frequency_spectrum_obj$frequency,
    amplitude = frequency_spectrum_obj$amplitude
  )

  # Test that beat_spectrum is empty
  expect_true(nrow(beat_spectrum) == 0)
})
