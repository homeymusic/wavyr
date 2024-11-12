# tests/testthat/test_spectrum.R

test_that("we can create a new spectrum with separate component and amplitude vectors", {
  # Create a spectrum object with separate vectors
  spectrum_obj <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
})

test_that("we can create a new spectrum with a list containing component and amplitude", {
  # Create a spectrum object with a list input
  spectrum_obj <- spectrum(
    list(c(1, 0.5, 0.33), c(1.0, 0.8, 0.5))
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
})

test_that("spectrum can calculate fundamental_cycle_length", {
  spectrum_obj <- spectrum(
    component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length (result might vary with fractions implementation)
  expect_true(is.numeric(spectrum_obj$fundamental_cycle_length))
})

test_that("spectrum can calculate fractions", {
  spectrum_obj <- spectrum(
    component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- spectrum_obj$fractions
  expect_equal(sort(fractions$num), sort(c(3, 1, 3)))
  expect_equal(sort(fractions$den), sort(c(1, 1, 2)))
})

test_that("spectrum can combine with another spectrum within tolerance", {
  # Create two spectrum objects
  spectrum1 <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )
  spectrum2 <- spectrum(
    component = c(1.0, 0.5001, 0.33),  # Close values to test tolerance
    amplitude = c(0.5, 0.4, 0.3)
  )

  # Combine spectrum1 and spectrum2 with a tolerance of 0.001
  combined_spectrum <- combine_spectra(
    spectrum1,
    spectrum2,
    tolerance = 0.001
  )

  # Expected combined component and amplitude values
  expected_components <- c(1.0, 0.5, 0.33)
  expected_amplitudes <- c(1.5, 1.2, 0.8)

  # Test the combined spectrum
  expect_s3_class(combined_spectrum, "spectrum")
  expect_equal(sort(combined_spectrum$component), sort(expected_components), tolerance = 0.01)
  expect_equal(sort(combined_spectrum$amplitude), sort(expected_amplitudes), tolerance = 0.01)
})

test_that("cycle length per component", {
  # Create a spectrum object with separate vectors
  spectrum_obj <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
  expect_equal(sort(spectrum_obj$cycle_length), sort(c(1, 1, 2)))
  expect_equal(spectrum_obj$fundamental_cycle_length, 2)
})

test_that("spectrum reduces closely spaced components within tolerance to a single component", {
  # Define components that are within FLOATING_POINT_TOLERANCE of each other
  base_frequency <- 100
  close_components <- c(
    base_frequency,
    base_frequency + FLOATING_POINT_TOLERANCE / 2,
    base_frequency + FLOATING_POINT_TOLERANCE / 3
  )
  amplitudes <- c(1.0, 0.8, 0.5)

  # Create a spectrum object with the close components and amplitudes
  spectrum_obj <- spectrum(
    component = close_components,
    amplitude = amplitudes
  )

  # Expected values
  expected_component <- base_frequency  # Representative component within tolerance
  expected_amplitude <- sum(amplitudes)  # Sum of amplitudes

  # Check that the resulting spectrum has only one component
  expect_equal(length(spectrum_obj$component), 1)
  expect_equal(spectrum_obj$component, expected_component, tolerance = FLOATING_POINT_TOLERANCE)
  expect_equal(spectrum_obj$amplitude, expected_amplitude)
})

test_that("spectrum plot works without overlay spectrum", {
  # Create a spectrum object without an overlay
  spectrum_obj <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Capture the plot with vdiffr to check visual output
  vdiffr::expect_doppelganger("spectrum plot without overlay", function() {
    .plot.spectrum(spectrum_obj, x_label = "Component", segment_color = "red")
  })
})

test_that("spectrum plot works with overlay spectrum", {
  # Create a main spectrum object
  main_spectrum <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create an overlay spectrum object
  overlay_spectrum <- spectrum(
    component = c(0.8, 0.4, 0.25),
    amplitude = c(0.6, 0.4, 0.3)
  )

  # Capture the plot with vdiffr to check visual output
  vdiffr::expect_doppelganger("spectrum with overlay", function() {
    .plot.spectrum(
      main_spectrum,
      x_label = "Component",
      segment_color = "red",
      overlay_spectrum = overlay_spectrum,
      overlay_spectrum_color = "blue"
    )
  })
})

test_that("spectrum plot raises error without overlay color", {
  # Create a main spectrum object
  main_spectrum <- spectrum(
    component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create an overlay spectrum object
  overlay_spectrum <- spectrum(
    component = c(0.8, 0.4, 0.25),
    amplitude = c(0.6, 0.4, 0.3)
  )

  # Expect an error when overlay_spectrum is provided without overlay_spectrum_color
  expect_error(
    .plot.spectrum(
      main_spectrum,
      x_label = "Component",
      segment_color = "red",
      overlay_spectrum = overlay_spectrum
    ),
    "overlay_spectrum_color must be specified if overlay_spectrum is provided"
  )
})
